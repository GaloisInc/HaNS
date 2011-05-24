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

module Network.TCP.LTS.Time
  ( tcp_update_timers
  )
where

import Foreign
import Foreign.C
import Data.Maybe
import Data.Map as Map
import Data.List as List
import Control.Monad

import Network.TCP.Type.Base
import Network.TCP.Type.Datagram as Datagram
import Network.TCP.Type.Syscall
import Network.TCP.Type.Socket
import Network.TCP.Type.Timer
import Hans.Layer.Tcp.Monad
import Network.TCP.Aux.SockMonad
import Network.TCP.Aux.Output
import Network.TCP.Aux.Misc
import Network.TCP.Aux.Param
import Network.TCP.LTS.Out

tcp_update_timers :: HMonad t () 
tcp_update_timers = 
  do h <- get_host
     when (clock h >= (fst $ next_timers h)) $ do 
        mapM update_fasttimer (keys (sock_map h))
        modify_host $ \h->h {next_timers = ( (fst $ next_timers h) + 200*1000, (snd $ next_timers h))}

     h <- get_host
     when (clock h >= (snd $ next_timers h)) $ do 
        mapM update_slowtimer (keys (sock_map h))
        modify_host $ \h->h {next_timers = ((fst $ next_timers h), (snd $ next_timers h) + 500*1000)}

update_fasttimer sid =
 runSMonad sid $ do
    sock <- get_sock
    let tcb = cb_rcv sock
    when (tt_delack tcb) $
       timer_tt_delack_1 sid sock

timer_tt_delack_1 sid sock =
 do modify_sock $ \sock-> sock { cb_rcv = (cb_rcv sock) { tt_delack = False } }
    tcp_output False

update_slowtimer sid =
 do h <- get_host
    sock <- lookup_sock sid
    let scb = cb_snd sock
        tcb = cb_time sock
        curr_time = clock h
    when (maybe_timed_expires curr_time (tt_rexmt scb)) $ 
       case tt_rexmt scb of
         Just (Timed (RexmtSyn, shift) tmr) -> timer_tt_rexmtsyn sid sock (shift)
         Just (Timed (Rexmt, shift) tmr) -> timer_tt_rexmt sid sock  (shift)
         Just (Timed (Persist, shift) tmr) -> timer_tt_persist sid sock  (shift)
         _ -> return ()

    when (maybe_timer_expires curr_time $ tt_keep tcb) $
       timer_tt_keep sid sock

    when (maybe_timer_expires curr_time $ tt_conn_est tcb) $
       timer_tt_conn_est sid

    when (maybe_timer_expires curr_time $ tt_2msl tcb) $
       timer_tt_2msl sid

    when (maybe_timer_expires curr_time $ tt_fin_wait_2 tcb) $
       timer_tt_fin_wait_2 sid

timer_tt_rexmtsyn sid sock shift =
  let tcb = cb sock in
  let scb = cb_snd sock in
  when (st sock == SYN_SENT) $ do
  if shift+1 >= tcp_maxrxtshift then tcp_drop_and_close sid else do
  h <- get_host
  let {
   (snd_cwnd_prev', snd_ssthresh_prev', t_badrxtwin') = 
     if shift==0 && (tf_srtt_valid $ t_rttinf scb) then 
        (snd_cwnd scb, snd_ssthresh tcb, create_timewindow (clock h) (t_srtt (t_rttinf scb) `div` 2 ) ())
     else
        (snd_cwnd_prev tcb, snd_ssthresh_prev tcb, t_badrxtwin $ cb_time sock);
   tf_req_tstmp' = if shift==2 then False else tf_req_tstmp tcb;
   req_r_scale'  = if shift==2 then Nothing else request_r_scale tcb;
   t_rttinf' = if shift+1 > tcp_maxrxtshift `div` 4 
                   then (t_rttinf scb) { tf_srtt_valid = False} else t_rttinf scb;
   newsock = sock 
    { cb_snd = scb 
       { tt_rexmt = start_tt_rexmtsyn (shift+1) False (t_rttinf scb) (clock h)
       , t_rttinf = t_rttinf' { t_lastshift = shift+1, t_wassyn = True }
       , snd_cwnd = t_maxseg tcb
       , t_dupacks = 0
       , t_rttseg = Nothing
       }
    , cb_time = (cb_time sock)
       { t_badrxtwin = t_badrxtwin'
       }
    , cb = tcb
       { tf_req_tstmp = tf_req_tstmp'
       , request_r_scale = req_r_scale'
       , snd_ssthresh = (t_maxseg tcb) * 
                        (max 2 (min (snd_wnd scb) (snd_cwnd scb) `div` (2 * (t_maxseg tcb))))
       , snd_cwnd_prev = snd_cwnd_prev'
       , snd_ssthresh_prev = snd_ssthresh_prev'
       }
    }
  }
  update_sock sid $ \_ -> newsock
  emit_segs [ TCPMessage $ make_syn_segment (clock h) newsock (ticks h)]

timer_tt_rexmt sid sock shift =
  let tcb = cb sock in
  let scb = cb_snd sock in
  when (st sock `notElem` [CLOSED,SYN_SENT,CLOSE_WAIT,FIN_WAIT_2,TIME_WAIT]) $
  if shift+1 > (if st sock == SYN_RECEIVED then tcp_synackmaxrxtshift else tcp_maxrxtshift)
  then tcp_drop_and_close sid else do 

  h <- get_host
  let {
    (snd_cwnd_prev', snd_ssthresh_prev', t_badrxtwin') =
       if shift+1==1 && tf_srtt_valid (t_rttinf scb) then
         (snd_cwnd scb, snd_ssthresh tcb, 
          create_timewindow (clock h) ( t_srtt (t_rttinf scb) `div` 2 ) () )
       else
         (snd_cwnd_prev tcb, snd_ssthresh_prev tcb, t_badrxtwin $ cb_time sock);
    t_rttinf' = if shift+1 > tcp_maxrxtshift `div` 4 then
                      (t_rttinf scb) { tf_srtt_valid = False
                                     , t_srtt = (t_srtt $ t_rttinf scb) `div` 4
                                     }
                    else t_rttinf scb;
    sock1 = sock 
      { cb_snd = scb 
        { tt_rexmt = start_tt_rexmt (shift+1) False (t_rttinf scb) (clock h)
        , t_rttinf = t_rttinf' { t_lastshift = shift + 1
                               , t_wassyn = False
                               }
        , snd_nxt = (snd_una scb)
        , t_rttseg = Nothing
        , snd_cwnd = t_maxseg tcb
        , t_dupacks = 0
        }
      , cb_time = (cb_time sock) { t_badrxtwin = t_badrxtwin' }
      , cb = tcb { snd_recover = snd_max scb
                 , snd_ssthresh = (t_maxseg tcb) * (max 2 
                     (min (snd_wnd scb) (snd_cwnd scb) `div` (2 * (t_maxseg tcb))))
                 , snd_cwnd_prev = snd_cwnd_prev'
                 , snd_ssthresh_prev = snd_ssthresh_prev'
                 }
      };
  }
  if st sock == SYN_RECEIVED then do

     emit_segs [ TCPMessage $ make_syn_ack_segment 
          (clock h) sock1 (local_addr tcb) (remote_addr tcb) (ticks h) ]
     update_sock sid $ \_ -> sock1 { cb_snd = (cb_snd sock1) 
          { snd_nxt = (snd_nxt $ cb_snd $ sock1) `seq_plus` 1 }}

   else if st sock == LISTEN then do

     let seg' = bsd_make_phantom_segment (clock h) sock1
                  (local_addr tcb) (remote_addr tcb) (ticks h) (cantsndmore tcb)
     emit_segs [ TCPMessage $ seg']
     update_sock sid $ \_ -> sock1 { cb_snd = (cb_snd sock1) 
          { tt_rexmt = if tcp_FIN seg' then tt_rexmt (cb_snd sock1) else Nothing } }

   else runSMonad sid $ do
        put_sock sock1
        tcp_output False

timer_tt_persist sid sock shift =
 runSMonad sid $ do
    h <- get_host_
    let scb = cb_snd sock
    put_sock $ sock { cb_snd = scb {tt_rexmt = start_tt_persist (shift+1) (t_rttinf scb) (clock h) }}
    tcp_output True

timer_tt_keep sid sock =
 do h <- get_host
    let tcb = cb sock
        scb = cb_snd sock
        rcb = cb_rcv sock
    let win_ = (rcv_wnd rcb `shiftR` (rcv_scale tcb))
    let ts'= if tf_doing_tstmp tcb then
                let ts_ecr' = case timewindow_val (clock h) (ts_recent $ cb_time sock) of
                                Just q -> q
                                Nothing -> Timestamp 0
                in
                Just ( (ticks h), ts_ecr')
              else
                Nothing
    let seg = TCPSegment
              { tcp_src = local_addr tcb
              , tcp_dst = remote_addr tcb
              , tcp_seq = snd_una scb `seq_minus` 1
              , tcp_ack = rcv_nxt rcb
              , tcp_URG = False
              , tcp_ACK = True
              , tcp_PSH = False
              , tcp_RST = False
              , tcp_SYN = False
              , tcp_FIN = False
              , tcp_win = win_
              , tcp_urp = 0
              , tcp_data = bufferchain_empty
              -- option: window scaling
              , tcp_ws = Nothing
              -- option: max segment size
              , tcp_mss = Nothing
              -- option: RFC1323
              , tcp_ts  = ts'
              }
    emit_segs [TCPMessage seg]
    update_sock sid $ \_ -> sock { cb_time = (cb_time sock) { tt_keep = Just (create_timer (clock h) tcptv_keepintvl) }
                                 , cb_rcv = rcb { last_ack_sent = tcp_ack seg }
                                 }
    return ()

timer_tt_conn_est sid =
    tcp_drop_and_close sid

timer_tt_2msl sid =
    tcp_close sid

timer_tt_fin_wait_2 sid =
    tcp_close sid

