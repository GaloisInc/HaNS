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

module Network.TCP.LTS.InActive where

import Foreign
import Data.Maybe
import Network.TCP.Type.Base
import Network.TCP.Aux.Misc
import Network.TCP.Aux.Param
import Network.TCP.Aux.Output
import Network.TCP.Type.Socket
import Network.TCP.Type.Datagram
import Network.TCP.Aux.SockMonad
import Network.TCP.LTS.User

deliver_in_2 :: TCPSegment -> SMonad t ()
deliver_in_2 seg = do
  sock <- get_sock
  h    <- get_host_
  --debug $ "deliver_in_3 " ++ (show seg)
  let tcb = cb sock
      scb = cb_snd sock
      rcb = cb_rcv sock
      acknum = SeqLocal (tcp_ack seg)
      seqnum = SeqForeign (tcp_seq seg)
  let { 
    -- window scaling
    (rcv_scale', snd_scale', tf_doing_ws') = 
       ( case (request_r_scale tcb, tcp_ws seg) of
          (Just rs, Just ss) -> (rs, ss, True)
          _ -> (0,0,False)
       );

    -- timestamping

    tf_rcvd_tstmp' = isJust $ tcp_ts seg;
    tf_doing_tstmp' =  tf_rcvd_tstmp' && (tf_req_tstmp tcb);
    -- mss negotiation
    ourmss = ( case (t_advmss tcb) of
                Nothing -> (t_maxseg tcb)
                Just v -> v
             );

    (_, _, t_maxseg'', snd_cwnd') = 
        calculate_buf_sizes ourmss (tcp_mss seg) Nothing False 
            (default_so_rcvbuf) (freebsd_so_sndbuf) tf_doing_tstmp';

    rcv_window = min tcp_maxwin default_so_rcvbuf;

    emission_time = 
      ( case tcp_ts seg of
         Just (_, ts_ecr) -> Just (ts_ecr `seq_minus` 1)
         Nothing -> case t_rttseg scb of
           Just (ts0, seq0) -> if acknum > seq0 then Just ts0 else Nothing
           Nothing -> Nothing;
      );

    t_rttseg' = ( case emission_time of
                    Nothing -> Nothing
                    Just _ -> t_rttseg scb );

    t_rttinf' = ( case emission_time of
                    Just emtime -> update_rtt
                      (fromIntegral (ticks h `seq_diff` emtime)*10*1000)
                      (t_rttinf scb)
                    Nothing -> t_rttinf scb );
    
    tt_rexmt' = if acknum == snd_max scb then Nothing else tt_rexmt scb;
    
    fin' = tcp_FIN seg;
    rcvq' = tcp_data seg;
    rcv_nxt' = seqnum `seq_plus` 1 `seq_plus` (if fin' then 1 else 0);
    rcv_wnd' = rcv_window - (bufc_length $ tcp_data seg);

    cantrcvmore' = if fin' then True else cantrcvmore tcb;

    new_st = if fin' then 
        if cantsndmore tcb then LAST_ACK else CLOSE_WAIT
     else 
        if cantsndmore tcb then
           if snd_max scb > iss tcb `seq_plus` 1 && acknum >= snd_max scb then
              FIN_WAIT_2
           else
              FIN_WAIT_1
        else
           ESTABLISHED;

    newsock = sock
     { st = new_st
     , cb_time = (cb_time sock) 
       { t_idletime = clock h
       , tt_keep = Just (create_timer (clock h) tcptv_keep_idle)
       , tt_conn_est = Nothing
       , ts_recent = case tcp_ts seg of
                       Nothing -> ts_recent $ cb_time sock
                       Just (ts_val, _) -> create_timewindow (clock h) dtsinval (ts_val)
       }
     , cb_snd = scb
       { tt_rexmt = tt_rexmt'
       , snd_una = acknum
       , snd_nxt = if cantsndmore tcb then acknum else snd_nxt scb
       , snd_max = if cantsndmore tcb && acknum > snd_max scb then acknum else snd_max scb
       , snd_wl1 = seqnum `seq_plus` 1
       , snd_wl2 = acknum
       , snd_wnd = fromIntegral (tcp_win seg) `shiftL` snd_scale'
       , snd_cwnd = if acknum > (iss tcb `seq_plus` 1) 
                    then min snd_cwnd' (tcp_maxwin `shiftL` snd_scale')
                    else snd_cwnd'
       , t_rttseg = t_rttseg'
       , t_rttinf = t_rttinf'
       }
     , cb_rcv = rcb
       { rcvq = rcvq'
       , tt_delack = False
       , rcv_nxt = rcv_nxt'
       , rcv_wnd = rcv_wnd'
       , tf_rxwin0sent = (rcv_wnd' == 0)
       , rcv_adv = rcv_nxt'
           `seq_plus` fromIntegral (( rcv_wnd' `shiftR` rcv_scale') `shiftL` rcv_scale')
       , last_ack_sent = rcv_nxt'
       }
     , cb = tcb
       { --local_addr = tcp_dst seg
         rcv_scale = rcv_scale'
       , snd_scale = snd_scale'
       , tf_doing_ws = tf_doing_ws'
       , irs = seqnum
       , t_maxseg = t_maxseg''
       , tf_req_tstmp = tf_doing_tstmp'
       , tf_doing_tstmp = tf_doing_tstmp'
       , cantrcvmore = cantrcvmore'
       }
     };
  }
  put_sock newsock
  emit_segs_ [ TCPMessage $ make_ack_segment (clock h) newsock 
        (cantsndmore tcb && acknum < (iss tcb `seq_plus` 2)) (ticks h)]
  tcp_wakeup
  return ()


