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

module Network.TCP.LTS.InPassive where

import Hans.Layer.Tcp.Monad
import Network.TCP.Aux.Misc
import Network.TCP.Aux.Output
import Network.TCP.Aux.Param
import Network.TCP.Aux.SockMonad
import Network.TCP.LTS.User
import Network.TCP.Type.Base
import Network.TCP.Type.Datagram
import Network.TCP.Type.Socket

import Control.Exception
import Control.Monad
import Data.List as List

tcp_deliver_syn_packet :: TCPSegment -> HMonad t ()
tcp_deliver_syn_packet seg = do
   -- precondition: sid does not exist
   -- try if seg matches a listening socket...
   let sidlisten = SocketID ((get_port $ tcp_dst seg), TCPAddr (IPAddr 0,0))
   --h <- get_host
   haslisten <- has_sock sidlisten
   if not haslisten then return () else do
      -- matches a socket...
      sock <- lookup_sock sidlisten
      if st sock /= LISTEN then return () else do
         -- now we find a listening socket maching incoming SYN=1 ACK=0 RST=0
         if accept_incoming_q0 (sock_listen sock)
            then deliver_in_1 sidlisten sock seg
            else return ()

deliver_in_1 :: SocketID -> TCPSocket t -> TCPSegment -> HMonad t ()
deliver_in_1 sid sock seg = 
  do let newsid = SocketID ((get_port $ tcp_dst seg), tcp_src seg)
     h <- get_host
     -- at this point, newsid is an unique socket id in the system...

     -- drop the first sid from q0 if needed, append newsid to q0.
     let lis1 = sock_listen sock
         should_drop = drop_from_q0 lis1
         drop_sid = head $ lis_q0 lis1
         oldq = lis_q0 lis1
         newq = if should_drop then tail oldq else oldq
         lis2 = lis1 { lis_q0 = newq++[newsid]}
     -- update listening socket (sid)
     update_sock sid $ \_ -> sock { sock_listen = lis2 }
     -- delete old socket if needed
     when should_drop $ tcp_close drop_sid    
 
     -- Create a new socket
     let advmss  = mssdflt -- todo: lookup interface mss
         advmss' = Nothing  -- not advertising MSS (todo: change it)

         --tf_rcvd_tstmp = case tcp_ts seg of Just _ -> True; Nothing -> False
         tf_doing_tstmp' = False -- not doing timestamping (todo: change it)

         --(rcvbufsize', sndbufsize', t_maxseg', snd_cwnd') =
         (_, _, t_maxseg', snd_cwnd') =
          calculate_buf_sizes advmss (tcp_mss seg) Nothing False
            (freebsd_so_rcvbuf) (freebsd_so_sndbuf) tf_doing_tstmp'

         tf_doing_ws' = False -- not doing window scaling (todo: change it)
         rcv_scale' = 0
         snd_scale' = 0
         rcv_window = min tcp_maxwin freebsd_so_rcvbuf

         newiss = SeqLocal 1000 -- beginning iss. (todo: add more randomness)
         t_rttseg' = Just (ticks h, newiss)
         seqnum = SeqForeign (tcp_seq seg)
         --acknum = SeqLocal (tcp_ack seg)
         ack' = seqnum `seq_plus` 1
         cb_time' = (cb_time sock)
               { tt_keep = Just (create_timer (clock h) tcptv_keep_idle)
               , ts_recent = case (tcp_ts seg) of
                   Nothing          -> ts_recent (cb_time sock)
                   Just (ts_val, _) ->
                               create_timewindow (clock h) (dtsinval) ts_val
               }
         cb_snd' = (cb_snd sock)
               { tt_rexmt = start_tt_rexmt 0 False (t_rttinf (cb_snd sock)) (clock h)
               , snd_una = newiss
               , snd_max = newiss `seq_plus` 1
               , snd_nxt = newiss `seq_plus` 1
               , snd_cwnd = snd_cwnd'
               , t_rttseg = t_rttseg'
               }
         cb_rcv' = (cb_rcv sock)
               { rcv_wnd = rcv_window
               , tf_rxwin0sent = (rcv_window == 0)
               , last_ack_sent = ack'
               , rcv_adv = ack' `seq_plus` fromIntegral rcv_window
               , rcv_nxt = ack'
               }
         cb' = (cb sock)
               { iss = newiss
               , irs = seqnum
               , rcv_up = seqnum `seq_plus` 1
               , t_maxseg = t_maxseg'
               , t_advmss = advmss'
               , rcv_scale = rcv_scale'
               , snd_scale = snd_scale'
               , tf_doing_ws = tf_doing_ws'
               , tf_req_tstmp = tf_doing_tstmp'
               , tf_doing_tstmp = tf_doing_tstmp'
               , local_addr = tcp_dst seg
               , remote_addr = tcp_src seg
               , self_id = newsid
               , parent_id = sid
               }
     -- create new socket (newsid)
     let newsock = initial_tcp_socket 
                   { st = SYN_RECEIVED
                   , cb = cb'
                   , cb_time = cb_time'
                   , cb_snd = cb_snd'
                   , cb_rcv = cb_rcv'
                   }
     insert_sock newsid newsock
     -- emit [SYN,ACK] packet
     emit_segs  [TCPMessage $ make_syn_ack_segment (clock h) newsock 
                                    (tcp_dst seg) (tcp_src seg) (ticks h) ]

-- After receiving ACK on SYN_RECEIVED, a connection is established.
-- Now we need to update the queues of the listening socket...
di3_socks_update :: SocketID -> HMonad t ()
di3_socks_update sid = do
    --h <- get_host
    -- precondition: sid exists
    newsock <- lookup_sock sid
    let tcb = cb newsock
        rcb = cb_rcv newsock
        sidlisten = parent_id tcb
    haslisten <- has_sock sidlisten
    assert (haslisten) return ()
    listensock <- lookup_sock sidlisten
    let lis1 = sock_listen listensock
    assert (sid `elem` (lis_q0 lis1)) return ()
    -- found the listening socket!
    if accept_incoming_q lis1 then do
       -- delete socket from q0
       -- move into completed queue
       let lis2 = lis1 { lis_q0 = List.delete sid (lis_q0 lis1)
                       , lis_q = sid : (lis_q lis1)
                       }
       let rcv_window = calculate_bsd_rcv_wnd newsock
       let newcb = (cb_rcv newsock) { rcv_wnd = rcv_window
                                    , rcv_adv = rcv_nxt rcb
                                        `seq_plus` fromIntegral (rcv_wnd rcb)
                                    }
       update_sock sidlisten $ \_ -> listensock { sock_listen = lis2 }
       update_sock sid       $ \_ -> newsock    { cb_rcv = newcb }
       runSMonad sidlisten $ tcp_wakeup
     else do
       -- delete socket from q0, backlog full -> delete socket
       let lis2 = lis1 { lis_q0 = List.delete sid (lis_q0 lis1) }
       update_sock sidlisten $ \_ -> listensock { sock_listen = lis2 }
       tcp_close sid
    --endif


