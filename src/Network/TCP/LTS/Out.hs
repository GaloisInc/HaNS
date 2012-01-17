{-# LANGUAGE ScopedTypeVariables #-}
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

module Network.TCP.LTS.Out
  ( tcp_output_all
  , tcp_output
  , tcp_close
  , tcp_drop_and_close
  )
where

import Hans.Message.Tcp

import Data.List as List
import Network.TCP.Aux.Output
import Network.TCP.Aux.Misc
import Network.TCP.Type.Base
import Network.TCP.Type.Socket
import Network.TCP.Aux.SockMonad
import Control.Monad
import Control.Exception
import Foreign
import Network.TCP.Type.Timer
import Network.TCP.Type.Datagram as Datagram
import Network.TCP.Aux.Param

tcp_output_all :: SMonad t ()
tcp_output_all  = do
    h <- get_host_
    sock <- get_sock
    let scb = cb_snd sock
        tcb = cb sock
    when ((st sock `elem` [ESTABLISHED, CLOSE_WAIT, FIN_WAIT_1, 
                          FIN_WAIT_2, CLOSING, LAST_ACK, TIME_WAIT]
          && (snd_una scb /= iss tcb)) -- does this make sense?
          || ( st sock `elem` [SYN_SENT, SYN_RECEIVED] && 
               cantsndmore tcb && (tf_shouldacknow $ cb_rcv sock))) $
     output_loop h sock
{-# INLINE tcp_output_all #-}

output_loop :: Host t -> TCPSocket t -> SMonad t ()
output_loop h sock =
   let (sock1, outsegs) = tcp_output_really (clock h) False (ticks h) sock in
   if List.null outsegs then 
      put_sock sock1
   else do
      --debug $ "tcp_output_all: " ++ (show outsegs)
      emit_segs_ $! outsegs
      output_loop h sock1
{-# INLINE output_loop #-}



tcp_output_really :: Time -> Bool -> Timestamp -> TCPSocket t
                  -> (TCPSocket t,[IPMessage])
tcp_output_really curr_time window_probe ts_val' tcp_sock =
    let tcb = cb tcp_sock
        scb = cb_snd tcp_sock
        rcb = cb_rcv tcp_sock
    in
    assert ((rcv_adv rcb) >= (rcv_nxt rcb)) $
    assert ((snd_nxt scb) >= (snd_una scb)) $
    let snd_cwnd' = if snd_max scb == snd_una scb && 
                       (t_idletime $ cb_time tcp_sock) - curr_time 
                       >= (computed_rxtcur $ t_rttinf scb)
                     then (t_maxseg tcb) * ss_fltsz -- has been idle for a while, slowstart
                     else snd_cwnd scb
        win0 = min (snd_wnd scb) snd_cwnd'
        win = if window_probe && win0==0 then 1 else win0
        snd_wnd_unused ::Int = win - ((snd_nxt scb) `seq_diff` (snd_una scb))
        syn_not_acked = (st tcp_sock `elem` [SYN_SENT, SYN_RECEIVED])
        fin_required = (cantsndmore tcb && st tcp_sock `notElem` [FIN_WAIT_2, TIME_WAIT])
        last_sndq_data_seq =
            snd_una scb `seq_plus` fromIntegral (bufc_length (sndq scb))
        last_sndq_data_and_fin_seq = last_sndq_data_seq
            `seq_plus` (if fin_required then 1 else 0  :: Word32)
            `seq_plus` (if syn_not_acked then 1 else 0 :: Word32)
        have_data_to_send = (snd_nxt scb) < last_sndq_data_seq
        have_data_or_fin_to_send = (snd_nxt scb) < last_sndq_data_and_fin_seq
        window_update_delta = (min (tcp_maxwin `shiftL` (rcv_scale tcb))
                                   (freebsd_so_rcvbuf - (bufc_length $ rcvq rcb))
                              ) - ( (rcv_adv rcb) `seq_diff` (rcv_nxt rcb))
        need_to_send_a_window_update =  (window_update_delta >= 2 * (t_maxseg tcb)) ||
                                        (2*window_update_delta >= freebsd_so_rcvbuf)
        do_output = ( have_data_or_fin_to_send && (if have_data_to_send then snd_wnd_unused>0 else True) )
                    || need_to_send_a_window_update -- sndurp tcp_sock /= Nothing
                    || tf_shouldacknow rcb
        cant_send = (not do_output) &&
                    (bufc_length (sndq scb) > 0 ) &&
                    mode_of (tt_rexmt scb) == Nothing
        window_shrunk = win==0 &&
                        snd_wnd_unused <0 &&
                        st tcp_sock /= SYN_SENT
        tcp_sock0 = if cant_send then 
                       tcp_sock { cb_snd = scb {tt_rexmt = start_tt_persist 0 (t_rttinf scb) curr_time}}
                    else if window_shrunk then 
                       tcp_sock { cb_snd = scb { 
                         tt_rexmt = case tt_rexmt scb of
                           Just(Timed (Persist, _) d ) ->
                             Just (Timed (Persist, 0) d)
                           _ -> start_tt_persist 0 (t_rttinf scb) curr_time
                       , snd_nxt = snd_una scb
                       }}
                    else tcp_sock
    in
    if  (not do_output) then (tcp_sock0, []) else
    ------------ really do it ---------------------------------------------
    let tcp_sock = tcp_sock0       -- fix this
        scb      = cb_snd tcp_sock -- fix this

        data' = bufferchain_drop (snd_nxt scb `seq_diff` (snd_una scb)) (sndq scb)
        data_to_send = bufferchain_take (min (snd_wnd_unused) ( t_maxseg tcb)) data'
        bFIN = fin_required
            && snd_nxt scb `seq_plus` fromIntegral (bufc_length data_to_send)
               >= last_sndq_data_seq
        bACK = if bFIN && st tcp_sock == SYN_SENT then False else True
        snd_nxt' = if bFIN &&
                   ((snd_nxt scb `seq_plus` fromIntegral (bufc_length data_to_send) == 
                    last_sndq_data_seq `seq_plus` 1 &&  snd_una scb /= iss tcb )
                    || snd_nxt scb `seq_diff` iss tcb == 2)
                   then snd_nxt scb `seq_minus` 1
                   else snd_nxt scb
        bPSH = bufc_length data_to_send > 0
            && snd_nxt scb `seq_plus` fromIntegral (bufc_length data_to_send)
               == last_sndq_data_seq
        rcv_wnd'' = calculate_bsd_rcv_wnd tcp_sock
        rcv_wnd' = max (rcv_adv rcb `seq_diff` (rcv_nxt rcb))
                       (min (tcp_maxwin `shiftL` (rcv_scale tcb))
                            (if rcv_wnd'' < (freebsd_so_rcvbuf `div` 4) && rcv_wnd'' < (t_maxseg tcb) 
                                then 0 else rcv_wnd''))
        want_tstmp = if st tcp_sock == SYN_SENT then tf_req_tstmp tcb else tf_doing_tstmp tcb
        ts_ = do_tcp_options curr_time want_tstmp (ts_recent $ cb_time tcp_sock) ts_val'
    in
    let win_ = rcv_wnd' `shiftR` (rcv_scale tcb)
        hdr = set_tcp_ts ts_ emptyTcpHeader
          { tcpSeqNum        = TcpSeqNum (seq_val snd_nxt')
          , tcpAckNum        = TcpAckNum (fseq_val (rcv_nxt rcb))
          , tcpAck           = bACK
          , tcpPsh           = bPSH
          , tcpFin           = bFIN
          , tcpWindow        = fromIntegral win_
          }
        seg = mkTCPSegment' (local_addr tcb) (remote_addr tcb) hdr data_to_send
        st' = if bFIN then
                 case st tcp_sock of
                  ESTABLISHED -> FIN_WAIT_1
                  CLOSE_WAIT  -> LAST_ACK
                  xxx         -> xxx
              else
                 st tcp_sock
        snd_nxt'' = snd_nxt' `seq_plus` fromIntegral (bufc_length data_to_send)
                             `seq_plus` (if bFIN then 1 else 0)
        snd_max'  = max (snd_max scb) snd_nxt''
        tt_rexmt' = if (mode_of (tt_rexmt scb) == Nothing ||
                        (mode_of (tt_rexmt scb) == Just Persist && not window_probe)) &&
                       snd_nxt'' > (snd_una scb) then
                              start_tt_rexmt 0 False (t_rttinf scb) curr_time
                    else if (window_probe  {-- || sndurp tcp_sock /= Nothing --} ) && win0 /= 0 &&  
                            mode_of (tt_rexmt scb) == Just Persist then
                              Nothing
                    else
                              tt_rexmt scb
        t_rttseg' = if t_rttseg scb == Nothing && (bufc_length data_to_send > 0 || bFIN) &&
                       snd_nxt'' > (snd_max scb) && not window_probe then
                       Just (ts_val', snd_nxt')
                    else
                       t_rttseg scb
        tcp_sock' = tcp_sock
                      { st = st'
                      , cb_snd = scb { tt_rexmt = tt_rexmt'
                                     , snd_cwnd = snd_cwnd'
                                     , t_rttseg = t_rttseg'
                                     , snd_max = snd_max'
                                     , snd_nxt = snd_nxt''
                                     }
                      , cb_rcv = rcb
                        { last_ack_sent   = rcv_nxt rcb
                        , rcv_adv         =
                          rcv_nxt rcb `seq_plus` fromIntegral rcv_wnd'
                        , tt_delack       = False
                        , rcv_wnd         = rcv_wnd'
                        , tf_rxwin0sent   = rcv_wnd' == 0
                        , tf_shouldacknow = False
                        }
                      }
        outsegs' = [TCPMessage seg]
    in
    (tcp_sock', outsegs')
{-# INLINE tcp_output_really #-}

{-# INLINE tcp_output #-}
tcp_output :: Bool -> SMonad t ()
tcp_output win_probe =
  do sock <- get_sock
     h <- get_host_
     let (newsock, segs) = tcp_output_really (clock h) win_probe (ticks h) sock
     put_sock newsock
     emit_segs_ segs
     --if List.null segs then return () else debug $ "tcp_output: " ++ (show segs)


  
