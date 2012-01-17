{--
Copyright (c) 2006, Peng Li
              2006, Stephan A. Zdancewic
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

* Neither the name of the copyright owners nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--}

module Network.TCP.LTS.In
  ( tcp_deliver_in_packet
  )
where

import Hans.Layer.Tcp.Monad
import Network.TCP.Aux.Misc
import Network.TCP.Aux.Output
import Network.TCP.Aux.Param
import Network.TCP.Aux.SockMonad
import Network.TCP.LTS.InActive
import Network.TCP.LTS.InData
import Network.TCP.LTS.InPassive
import Network.TCP.LTS.Out
import Network.TCP.LTS.User
import Network.TCP.Type.Base
import Network.TCP.Type.Datagram
import Network.TCP.Type.Socket

import Control.Exception
import Control.Monad
import Data.List as List
import Foreign


tcp_deliver_in_packet :: TCPSegment -> HMonad t ()
tcp_deliver_in_packet seg = do
  let sid = SocketID (get_port (tcp_dst seg), tcp_src seg)
  ok <- has_sock sid
  if ok
     then tcp_deliver_packet_to_sock sid seg
     else if tcp_SYN seg && (not $ tcp_ACK seg) && (not $ tcp_RST seg) 
             then tcp_deliver_syn_packet seg
             else emit_segs $ dropwithreset seg

-- Note: if there exists a socket in TIME_WAIT state, and an SYN
-- packet matches it, the SYN packet will always be delivered to this
-- socket; it will never be delivered to a listening socket.  This
-- makes the implementation simpler...

--pre-condition: sid exists
tcp_deliver_packet_to_sock :: SocketID -> TCPSegment -> HMonad t ()
tcp_deliver_packet_to_sock sid seg =
  do h <- get_host
     sock <- lookup_sock sid
     let tcb = cb sock
         rcb = cb_rcv sock
         scb = cb_snd sock
         seqnum = SeqForeign (tcp_seq seg)
         acknum = SeqLocal (tcp_ack seg)

     success <- header_prediction seg h sid sock tcb rcb scb seqnum acknum 
     when (not success) $
      case st sock of
       CLOSED   -> assert (False) return ()
       LISTEN   -> assert (False) return ()
       SYN_SENT -> let goodack = (iss tcb) < acknum && acknum <= (snd_max scb) in
                   if tcp_RST seg then 
                      when (tcp_ACK seg && goodack) $ tcp_close sid 
                   else 
                      if tcp_SYN seg && tcp_ACK seg then
                         if goodack then runSMonad sid $ deliver_in_2 seg
                         else emit_segs $ dropwithreset seg
                       else return ()
       SYN_RECEIVED ->
                   let invalidack = acknum <= snd_una scb || acknum > snd_max scb in
                   if tcp_RST seg then 
                      tcp_close sid
                   else if tcp_SYN seg || not (tcp_ACK seg) then  -- check with spec?
                      return ()
                   else if invalidack || (seqnum < (irs tcb)) then
                      return ()
                   else do
                      sock' <- runSMonad sid $ deliver_in_3 seg
                      if st sock' == CLOSED then
                          tcp_close sid
                       else when (st sock' /= SYN_RECEIVED) $
                          di3_socks_update sid
       _        -> if tcp_RST seg then 
                      when (st sock /= TIME_WAIT) $ tcp_close sid
                   else if tcp_SYN seg then 
                      when (st sock==TIME_WAIT) $ emit_segs $ dropwithreset seg
                   else  
                      if st sock `elem` [FIN_WAIT_1, CLOSING, LAST_ACK, FIN_WAIT_2, TIME_WAIT]  
                         && seqnum
                             `seq_plus` fromIntegral (bufc_length (tcp_data seg))
                           > rcv_nxt rcb
                        then return () -- data coming into closing socket?
                        else do sock' <- runSMonad sid $ deliver_in_3 seg
                                --debug $ (show $ st sock')
                                when (st sock' == CLOSED) $ tcp_close sid

{-# INLINE header_prediction #-}
header_prediction :: TCPSegment -> Host t' -> SocketID -> TCPSocket t''
                  -> TCBMisc -> TCBReceiving -> TCBSending
                  -> SeqForeign -> SeqLocal -> HMonad t Bool
header_prediction seg h sid sock tcb rcb scb seqnum acknum =
 if st sock == ESTABLISHED
     && not (tcp_SYN seg) 
     && not (tcp_FIN seg)
     && not (tcp_URG seg)
     && not (tcp_RST seg)
     && tcp_ACK seg
     && seqnum == rcv_nxt rcb
     && snd_wnd scb == fromIntegral (tcp_win seg) `shiftL` snd_scale tcb
     && snd_max scb == snd_nxt scb
  then if bufc_length (tcp_data seg) == 0
          && acknum > (snd_una scb)
          && acknum <= (snd_max scb)
          && snd_cwnd scb >= snd_wnd scb
          && t_dupacks scb < 3
       then do -- pure ack for outstanding data
            --------------------------------------------------------------------------------
            --debug $ "prediction 2.1!"
            let emission_time = case (tcp_ts seg, t_rttseg scb) of
                                (Just (_, ts_ecr), _ ) -> Just (ts_ecr `seq_minus` 1)
                                (Nothing, Just (ts0, seq0)) -> if acknum > seq0 then Just ts0 else Nothing
                                (Nothing, Nothing) -> Nothing
            let t_rttinf' = case emission_time of
                            Just emtime -> assert ((ticks h) >= emtime) $
                                           update_rtt ( fromIntegral ((ticks h) `seq_diff` emtime)*10000 ) (t_rttinf scb)
                            Nothing -> t_rttinf scb
            let tt_rexmt' = if acknum == snd_max scb then
                               Nothing
                            else case mode_of (tt_rexmt scb) of
                                 Nothing ->    start_tt_rexmt 0 True t_rttinf' (clock h)
                                 Just Rexmt -> start_tt_rexmt 0 True t_rttinf' (clock h)
                                 _ -> tt_rexmt scb
            let acked = acknum `seq_diff` (snd_una scb)
            let snd_wnd' = snd_wnd scb - acked
            let sndq' = bufferchain_drop acked (sndq scb)
            runSMonad sid $ do
               modify_sock $ \s -> s { cb_snd = scb 
                  { sndq = sndq'
                  , t_dupacks = 0
                  , t_rttinf = t_rttinf'
                  , tt_rexmt = tt_rexmt'
                  , t_rttseg = if emission_time == Nothing then t_rttseg scb else Nothing
                  , snd_cwnd = expand_cwnd (snd_ssthresh tcb) 
                                         (t_maxseg tcb) 
                                         (tcp_maxwin `shiftL` (snd_scale tcb))
                                         (snd_cwnd scb)
                  , snd_wnd = snd_wnd'
                  , snd_una = acknum
                  --, snd_nxt = max acknum (snd_nxt scb)
                  }
                }
               tcp_wakeup
               tcp_output_all
            return True
            --------------------------------------------------------------------------------
       else if acknum == snd_una scb
               && List.null (t_segq rcb)
               && bufc_length (tcp_data seg) < (freebsd_so_rcvbuf - (bufc_length $ rcvq rcb))
       then do -- pure in-sequence data packet 
            --------------------------------------------------------------------------------
            return False
            --------------------------------------------------------------------------------
       else do
            -- debug $ "predictions 2.1, 2.2 fail!"
            return False
  else do
       -- debug $ "prediction 1 fail!" ++ (show $ snd_wnd tcb) 
       --      ++ " " ++ (show  (tcp_win seg)) ++ " " ++ (show $ snd_scale tcb)
       return False
     
