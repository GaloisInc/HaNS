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

module Network.TCP.LTS.InData where

import Foreign
import Foreign.C
import Control.Exception
import Control.Monad
import Data.List as List

import Network.TCP.Type.Base
import Network.TCP.Type.Timer
import Network.TCP.Type.Socket
import Network.TCP.Type.Datagram as Datagram
import Network.TCP.Type.Syscall

import Network.TCP.Aux.Param
import Network.TCP.Aux.Misc
import Network.TCP.Aux.Output
import Hans.Layer.Tcp.Monad
import Network.TCP.Aux.SockMonad

import Network.TCP.LTS.User
import Network.TCP.LTS.Out

deliver_in_3 seg = 
 do sock <- get_sock
    h <- get_host_
    --debug $ "deliver_in_3 " ++ (show seg)
    let tcb = cb sock
        scb = cb_snd sock
        acknum = SeqLocal (tcp_ack seg)
        seqnum = SeqForeign (tcp_seq seg) `seq_plus`
                 if tcp_SYN seg then 1 else 0
        seg_win = fromIntegral (tcp_win seg) `shiftL` (snd_scale tcb)
    let wesentafin = (snd_max scb) > (snd_una scb `seq_plus` (bufc_length $ sndq scb))
        ourfinisacked = wesentafin && tcp_ACK seg && acknum >= (snd_max scb)

    -- update idle time
    -- seqnum bound checking
    drop_it <- di3_topstuff seg seqnum acknum h 
    when (not drop_it) $ do
      -- acknum bound checking
      -- fast retransmit
      -- correct bad retransmit
      -- update send queue
      ack_ok <- di3_ackstuff seg seqnum acknum seg_win h ourfinisacked
      when ack_ok $ do
        -- update send window
        -- receive data
        fin_reass <- di3_datastuff seg seqnum acknum seg_win h ourfinisacked
        -- update socket state
        di3_ststuff fin_reass h ourfinisacked acknum
    tcp_wakeup
    tcp_output_all
    get_sock

{-# INLINE di3_topstuff #-}
di3_topstuff seg seqnum acknum h = 
 do sock <- get_sock
    let tcb = cb sock
        scb = cb_snd sock
        rcb = cb_rcv sock
    let rseq = seqnum `seq_plus` (bufc_length $ tcp_data seg)
    let seg_ts = tcp_ts seg
    -- PAWS check: -- todo
    let paws_failed = False
    let rcv_wnd' = calculate_bsd_rcv_wnd sock
    let segment_off_right_hand_edge =
             (seqnum >= (rcv_nxt rcb `seq_plus` rcv_wnd'))
          && (rseq   >  (rcv_nxt rcb `seq_plus` rcv_wnd'))
          && (rcv_wnd' /= 0)
    let drop_it = paws_failed || segment_off_right_hand_edge
    let Just seg_ts_val = seg_ts
    let (tt_keep', tt_fin_wait_2') = update_idle (clock h) sock
    let ts_recent'' = if not drop_it && seg_ts /= Nothing && seqnum <= (last_ack_sent rcb)
                       then create_timewindow (clock h) dtsinval (fst $ seg_ts_val)
                       else ts_recent $ cb_time sock
    modify_cb_time $ \t -> t { tt_keep = tt_keep'
                             , tt_fin_wait_2 = tt_fin_wait_2'
                             , t_idletime = clock h
                             , ts_recent = ts_recent''
                             }
    return drop_it

{-# INLINE di3_ackstuff #-}
di3_ackstuff seg seqnum acknum seg_win h ourfinisacked = 
 do sock <- get_sock
    let scb = cb_snd sock
    if acknum > snd_max scb then return False
     else if acknum > snd_una scb 
      then di3_newackstuff sock seg acknum h ourfinisacked
      else di3_oldackstuff sock seg seqnum acknum seg_win h

{-# INLINE di3_oldackstuff #-}
di3_oldackstuff sock seg seqnum acknum seg_win h = 
  let tcb = cb sock
      scb = cb_snd sock
      rcb = cb_rcv sock in
  let has_data = bufc_length (tcp_data seg) > 0
                 && (rcv_nxt rcb) < (seqnum `seq_plus` (bufc_length $ tcp_data seg))
                 && seqnum < ( (rcv_nxt rcb) `seq_plus` (rcv_wnd rcb)) in
  let maybe_dup_ack = not has_data 
                      && seg_win == (snd_wnd scb)
                      && mode_of (tt_rexmt scb) == Just Rexmt in
  if not maybe_dup_ack then do
       modify_cb_snd $ \c -> c { t_dupacks = 0 }
       return True
  else
    let t_dupacks' = t_dupacks scb + 1 in
    if acknum < (snd_una scb) then
         do modify_cb_snd $ \c -> c { t_dupacks = 0}
            return False 
    else if t_dupacks' < 3 then
         do modify_cb_snd $ \c -> c { t_dupacks = t_dupacks'}
            return True -- in case FIN is set
    else if t_dupacks' > 3 || (t_dupacks' == 3 && tcp_do_newreno && acknum < (snd_recover tcb)) then
         do modify_cb_snd $ \c -> c { t_dupacks = if t_dupacks' == 3 then 0 else t_dupacks'
                                    , snd_cwnd = (snd_cwnd scb) + (t_maxseg tcb)
                                    }
            tcp_output False
            return False
    else -- t_dupacks' == 3 && not (tcp_do_newreno && acknum < (snd_recover tcb))
         do modify_cb_snd $ \c -> c { t_dupacks = t_dupacks'
                                    , tt_rexmt = Nothing
                                    , t_rttseg = Nothing
                                    , snd_nxt = acknum
                                    , snd_cwnd = t_maxseg tcb
                                    } 
            modify_cb $ \c -> c { snd_ssthresh = (max 2 ( (min (snd_wnd scb) (snd_cwnd scb)) 
                                                          `div` 2 `div` (t_maxseg tcb))) 
                                                 * (t_maxseg tcb)
                                , snd_recover = if tcp_do_newreno then snd_max scb else snd_recover c
                                }
            tcp_output False
            modify_cb_snd $ \c -> c { snd_cwnd = (snd_ssthresh tcb) + (t_maxseg tcb) * t_dupacks'
                                    , snd_nxt = max (snd_nxt scb) (snd_nxt c)
                                    }
            return False
 
{-# INLINE di3_newackstuff #-}
di3_newackstuff sock seg acknum h ourfinisacked =
 do let seg_ts = tcp_ts seg
    let tcb = cb sock
        scb = cb_snd sock
        rcb = cb_rcv sock
    if (not tcp_do_newreno) || t_dupacks scb < 3 then
       modify_cb_snd $ \c->c { t_dupacks = 0
                             , snd_cwnd = if t_dupacks c >= 3 
                                          then min (snd_cwnd c) (snd_ssthresh tcb)
                                          else snd_cwnd c }
       -- below: tcp_do_newreno && t_dupacks scb >= 3
     else if acknum < (snd_recover tcb) then
       do modify_cb_snd $ \c -> c { tt_rexmt = Nothing
                                  , t_rttseg = Nothing
                                  , snd_nxt = acknum
                                  , snd_cwnd = t_maxseg tcb }
          tcp_output False
          modify_cb_snd $ \c->c{ snd_cwnd = (snd_cwnd c-(acknum `seq_diff` (snd_una c))+(t_maxseg tcb))
                               , snd_nxt = snd_nxt scb }
     else --acknum >= snd_recover tcb
       modify_cb_snd $ \c -> c { t_dupacks = 0
                               , snd_cwnd = if   snd_max c `seq_diff` acknum < (snd_ssthresh tcb) 
                                            then snd_max c `seq_diff` acknum + (t_maxseg tcb)
                                            else snd_ssthresh tcb }

    let revert_rexmt = mode_of (tt_rexmt scb) `elem` [ Just Rexmt, Just RexmtSyn ] 
                       && shift_of (tt_rexmt scb) == 1
                       && timewindow_open (clock h) (t_badrxtwin $ cb_time sock)
    when revert_rexmt $ do
       modify_cb_snd $ \c -> c { snd_cwnd = snd_cwnd_prev tcb
                               , snd_nxt = snd_max scb
                               }
       modify_cb_time $ \c -> c { t_badrxtwin = Nothing }
       modify_cb $ \c -> c { snd_ssthresh = snd_ssthresh_prev tcb }

    -- to understand: timestamping
    let emission_time = case (seg_ts, t_rttseg scb) of
                        (Just (ts_val, ts_ecr), _ ) -> Just (ts_ecr `seq_minus` 1)
                        (Nothing, Just (ts0, seq0)) -> if acknum > seq0 then Just ts0 else Nothing
                        (Nothing, Nothing) -> Nothing
    -- to understand: rtt update
    let t_rttinf' = case emission_time of
                    Just emtime -> assert ((ticks h) >= emtime) $
                                   update_rtt ( ((ticks h) `seq_diff` emtime)*10*1000 ) (t_rttinf scb)
                    Nothing -> t_rttinf scb
    let tt_rexmt' = if acknum == snd_max scb then
                       Nothing
                    else case mode_of (tt_rexmt scb) of
                         Nothing ->    start_tt_rexmt 0 True t_rttinf' (clock h)
                         Just Rexmt -> start_tt_rexmt 0 True t_rttinf' (clock h)
                         _ -> tt_rexmt scb
    let (snd_wnd', sndq') = if ourfinisacked then
                               (snd_wnd scb - (bufc_length $ sndq scb), bufferchain_empty)
                            else
                               (snd_wnd scb - (acknum `seq_diff` (snd_una scb)),
                                bufferchain_drop (acknum `seq_diff` (snd_una scb)) (sndq scb))

    modify_cb_snd $ \c -> c { t_rttinf = t_rttinf'
                            , tt_rexmt = tt_rexmt'
                            , t_rttseg    = if emission_time == Nothing then t_rttseg c else Nothing
                            , snd_cwnd = if not tcp_do_newreno || t_dupacks scb == 0 then
                                            expand_cwnd (snd_ssthresh tcb) 
                                                        (t_maxseg tcb) 
                                                        (tcp_maxwin `shiftL` (snd_scale tcb))
                                                        (snd_cwnd c)
                                         else snd_cwnd c
                            , snd_wnd = snd_wnd'
                            , snd_una = acknum
                            , snd_nxt = max acknum (snd_nxt c)
                            , sndq = sndq'
                            }

    when (st sock == TIME_WAIT) $
       modify_cb_time $ \c -> c { tt_2msl = Just (create_timer (clock h) (2*tcptv_msl))}

    if (st sock == LAST_ACK) && ourfinisacked then do
       modify_sock tcp_close_temp
       return False
     else return True

{-# INLINE di3_datastuff #-}
di3_datastuff seg seqnum acknum seg_win h ourfinisacked = do 
    sock <- get_sock
    let tcb = cb sock
        scb = cb_snd sock
        rcb = cb_rcv sock
    let update_send_window = 
         tcp_ACK seg
         && seqnum <= ( (rcv_nxt rcb) `seq_plus` (rcv_wnd rcb) )
         && ( snd_wl1 scb < seqnum 
              || ( snd_wl1 scb == seqnum 
                   && ( snd_wl2 scb < acknum
                        || ( snd_wl2 scb == acknum && seg_win > snd_wnd scb )
                      )
                 )
              || (st sock == SYN_RECEIVED && not (tcp_FIN seg) )
            )
    let seq_trimmed = max seqnum (min (rcv_nxt rcb) (seqnum `seq_plus` (bufc_length $ tcp_data seg)))
    when update_send_window $
       --debug $ "send window updated"
       modify_cb_snd $ \c -> c { snd_wnd = seg_win
                               , snd_wl1 = seq_trimmed
                               , snd_wl2 = acknum
                               }
    if st sock == TIME_WAIT || (st sock == CLOSING && ourfinisacked) 
     then do modify_cb $ \c -> c { rcv_up = max (rcv_up c) (rcv_nxt rcb) }
             return False
     else di3_datastuff_really seg seqnum acknum seg_win h

{-# INLINE di3_datastuff_really #-}
di3_datastuff_really seg seqnum acknum seg_win h = 
 do let dat = tcp_data seg
    sock <- get_sock
    let tcb = cb sock
        scb = cb_snd sock
        rcb = cb_rcv sock
    
    let trim_amt_left = if rcv_nxt rcb > seqnum
                        then min (rcv_nxt rcb `seq_diff` seqnum) (bufc_length dat)
                        else 0
        data_trimmed_left = bufferchain_drop trim_amt_left dat                                               
        seq_trimmed = seqnum `seq_plus` trim_amt_left
    let data_trimmed_left_right = bufferchain_take (rcv_wnd rcb) data_trimmed_left
        fin_trimmed = if bufc_length data_trimmed_left_right == 
                         bufc_length data_trimmed_left then tcp_FIN seg else False
    let rseg = TCPReassSegment { trs_seq = seq_trimmed
                               , trs_FIN = fin_trimmed
                               , trs_data = data_trimmed_left_right
                               }
    -- processing incoming data
    if seq_trimmed == rcv_nxt rcb
       && seq_trimmed `seq_plus` (bufc_length data_trimmed_left_right) 
           `seq_plus` (if fin_trimmed then 1 else 0) > (rcv_nxt rcb)
       && rcv_wnd rcb > 0 
     then do
       -- case 1: reassambling possible
       let have_stuff_to_ack = bufc_length data_trimmed_left_right >0 || fin_trimmed
       let delay_ack = st sock `elem` [ESTABLISHED, CLOSE_WAIT, FIN_WAIT_1, FIN_WAIT_2, CLOSING, LAST_ACK]
                       && have_stuff_to_ack && not fin_trimmed && List.null (t_segq rcb)
                       && not (tf_rxwin0sent rcb)
                       && tt_delack rcb == False
       let rsegq = rseg:(t_segq rcb)
       let (data_reass, rcv_nxt', fin_reass0, t_segq') = tcp_reass (rcv_nxt rcb) rsegq
       let rcvq' = bufferchain_concat (rcvq rcb) data_reass
       let rcv_wnd' = rcv_wnd rcb - (bufc_length data_reass)
       modify_cb_rcv $ \c -> c 
         { tt_delack = if delay_ack then True else tt_delack c
         , tf_shouldacknow = if have_stuff_to_ack then not delay_ack else tf_shouldacknow c
         , t_segq = t_segq'
         , rcv_nxt = rcv_nxt'
         , rcv_wnd = rcv_wnd'
         , rcvq = rcvq'
         }
       return fin_reass0
     else if seq_trimmed > (rcv_nxt rcb)
             && seq_trimmed < ((rcv_nxt rcb) `seq_plus` (rcv_wnd rcb))
             && bufc_length data_trimmed_left_right + (if fin_trimmed then 1 else 0) > 0 
             && rcv_wnd rcb > 0 
     then do
       -- case 2: wait for future reassambling
       modify_cb_rcv $ \c -> c { t_segq = rseg:(t_segq c)
                               , tf_shouldacknow = True
                               }
       return False
     else if tcp_ACK seg && seq_trimmed == rcv_nxt rcb 
              && bufc_length dat + (if tcp_FIN seg then 1 else 0) == 0 then
       -- case 3: no data
       return False
     else do
       -- case 4: other cases... maybe windows is closed
         modify_cb_rcv $ \c -> c { tf_shouldacknow = True }
         return False

{-# INLINE di3_ststuff #-}
di3_ststuff fin_reass h ourfinisacked acknum =
 do sock <- get_sock
    let tcb = cb sock
    let enter_TIME_WAIT = do
          modify_sock $ \s -> s { st = TIME_WAIT }
          modify_cb_time $ \c -> c { tt_2msl = Just (create_timer (clock h) (2*tcptv_msl))
                                   , tt_keep = Nothing
                                   , tt_conn_est = Nothing
                                   , tt_fin_wait_2 = Nothing
                                   }
          modify_cb_snd $ \c -> c { tt_rexmt = Nothing }
          modify_cb_rcv $ \c -> c { tt_delack = False }

    when fin_reass $
       modify_cb $ \s -> s { cantrcvmore = True }
    
    case (st sock, fin_reass) of
     (SYN_RECEIVED,False) -> when (acknum >= (iss tcb) `seq_plus` 1 ) $ 
                               modify_sock $ \s -> s 
                                 { st = if not (cantsndmore tcb) then ESTABLISHED else
                                         if ourfinisacked then FIN_WAIT_2 else FIN_WAIT_1
                                 }
     (SYN_RECEIVED, True) -> modify_sock $ \s -> s { st = CLOSE_WAIT }
     (ESTABLISHED, False) -> return ()
     (ESTABLISHED, True)  -> modify_sock $ \s -> s { st = CLOSE_WAIT }
     (CLOSE_WAIT, _ )     -> return ()
     (FIN_WAIT_1, False)  -> when ourfinisacked $ do
                               modify_sock $ \s -> s { st = FIN_WAIT_2 }
                               when (cantrcvmore tcb) $
                                 modify_cb_time $ \c -> c { tt_fin_wait_2 =
                                    Just (create_timer (clock h) (tcptv_maxidle)) }
     (FIN_WAIT_1, True)   -> if ourfinisacked then enter_TIME_WAIT 
                              else modify_sock $ \s->s { st=CLOSING }
     (FIN_WAIT_2, False)  -> return ()
     (FIN_WAIT_2, True)   -> enter_TIME_WAIT
     (CLOSING, _)         -> when ourfinisacked enter_TIME_WAIT
     (LAST_ACK, False)    -> return ()
     (LAST_ACK, True)     -> error "di3_ststuff"
     (TIME_WAIT, _ )      -> return ()
