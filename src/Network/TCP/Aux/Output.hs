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

module Network.TCP.Aux.Output where

import Hans.Message.Tcp

import Network.TCP.Type.Base
import Network.TCP.Type.Timer
import Network.TCP.Type.Datagram as Datagram
import Network.TCP.Type.Socket
import Network.TCP.Type.Syscall
import Network.TCP.Aux.Param
import Network.TCP.Aux.Misc
import Hans.Layer.Tcp.Monad
import Foreign
import Control.Exception
import Control.Monad

make_syn_segment :: Time -> TCPSocket t -> Timestamp -> TCPSegment
make_syn_segment curr_time sock (ts_val::Timestamp) = 
    let ws = request_r_scale $ cb sock -- should assert it's <= tcp_maxwinscale ?
        mss = t_advmss $ cb $ sock
        ts = do_tcp_options curr_time (tf_req_tstmp $ cb sock) (ts_recent $ cb_time $ sock ) ts_val
        hdr =
          set_tcp_mss mss $
          set_tcp_ws  ws  $
          set_tcp_ts  ts  $ emptyTcpHeader
            { tcpSeqNum        = TcpSeqNum (seq_val (iss (cb sock)))
            , tcpAckNum        = TcpAckNum 0
            , tcpSyn           = True
            , tcpWindow        = fromIntegral (rcv_wnd (cb_rcv sock))
            }
    in mkTCPSegment'
           (local_addr (cb sock)) (remote_addr (cb sock)) hdr bufferchain_empty

make_syn_ack_segment curr_time sock (addrfrom::TCPAddr) (addrto::TCPAddr) (ts_val::Timestamp) = 
    let urp_any = 0
        tcb = cb sock
        win = rcv_wnd (cb_rcv sock) -- `shiftR` (rcv_scale $ cb sock)
        ws = if tf_doing_ws tcb then Just (rcv_scale tcb) else Nothing
        mss = t_advmss tcb 
        ts = do_tcp_options curr_time (tf_req_tstmp tcb) (ts_recent $ cb_time sock) ts_val
        hdr =
          set_tcp_mss mss $
          set_tcp_ws  ws  $
          set_tcp_ts  ts  $ emptyTcpHeader
            { tcpSeqNum        = TcpSeqNum (seq_val (iss tcb))
            , tcpAckNum        = TcpAckNum (fseq_val (rcv_nxt (cb_rcv sock)))
            , tcpAck           = True
            , tcpSyn           = True
            , tcpWindow        = fromIntegral win
            , tcpUrgentPointer = urp_any
            }
    in mkTCPSegment' addrfrom addrto hdr bufferchain_empty

make_ack_segment curr_time sock (fin::Bool) (ts_val::Timestamp) = 
    let urp_garbage = 0
        tcb = cb sock
        win = (rcv_wnd $ cb_rcv sock) `shiftR` (rcv_scale tcb)
        ts = do_tcp_options curr_time (tf_req_tstmp tcb) (ts_recent $ cb_time sock) ts_val
        sn | fin       = snd_una (cb_snd sock)
           | otherwise = snd_nxt (cb_snd sock)
        hdr =
          set_tcp_ts ts $ emptyTcpHeader
            { tcpSeqNum        = TcpSeqNum (seq_val sn)
            , tcpAckNum        = TcpAckNum (fseq_val (rcv_nxt (cb_rcv sock)))
            , tcpAck           = True
            , tcpFin           = fin
            , tcpWindow        = fromIntegral win
            , tcpUrgentPointer = urp_garbage
            }
    in mkTCPSegment' (local_addr tcb) (remote_addr tcb) hdr bufferchain_empty

bsd_make_phantom_segment curr_time sock (addrfrom::TCPAddr) (addrto::TCPAddr) (ts_val::Timestamp) (cantsendmore::Bool) = 
    let urp_garbage = 0
        tcb = cb sock
        scb = cb_snd sock
        rcb = cb_rcv sock
        win = (rcv_wnd rcb) `shiftR` (rcv_scale tcb)
        fin = (cantsendmore && seq_lt (snd_una scb) (seq_minus (snd_max scb) 1))
        ts = do_tcp_options curr_time (tf_req_tstmp tcb) (ts_recent $ cb_time sock) ts_val
        sn | fin       = snd_una scb
           | otherwise = snd_max scb
        hdr =
          set_tcp_ts ts emptyTcpHeader
            { tcpSourcePort    = TcpPort 0
            , tcpDestPort      = TcpPort 0
            , tcpSeqNum        = TcpSeqNum (seq_val sn)
            , tcpAckNum        = TcpAckNum (fseq_val (rcv_nxt rcb))
            , tcpFin           = fin
            , tcpWindow        = fromIntegral win
            , tcpUrgentPointer = urp_garbage
            }
    in
    mkTCPSegment' addrfrom addrto hdr bufferchain_empty

make_rst_segment_from_cb sock (addrfrom::TCPAddr) (addrto::TCPAddr) = 
  let hdr = emptyTcpHeader
        { tcpSourcePort    = TcpPort 0
        , tcpDestPort      = TcpPort 0
        , tcpSeqNum        = TcpSeqNum (seq_val (snd_nxt (cb_snd sock)))
        , tcpAckNum        = TcpAckNum (fseq_val (rcv_nxt (cb_rcv sock)))
        , tcpAck           = False
        , tcpRst           = False
        }
   in mkTCPSegment' addrfrom addrto hdr bufferchain_empty

make_rst_segment_from_seg (seg::TCPSegment) =
    let tcp_ACK' = not (tcp_ACK seg)
        seq' = if tcp_ACK seg then tcp_ack seg else 0
        ack' = if tcp_ACK'
                  then let s1 = tcp_seq seg
                       in  s1 `seq_plus` bufc_length (tcp_data seg)
                              `seq_plus` (if tcp_SYN seg then 1 else 0)
                  else 0


        hdr = emptyTcpHeader
          { tcpSeqNum        = TcpSeqNum seq'
          , tcpAckNum        = TcpAckNum ack'
          , tcpAck           = tcp_ACK'
          , tcpRst           = True
          }
    in
    mkTCPSegment' (tcp_src seg) (tcp_dst seg) hdr bufferchain_empty


dropwithreset (seg::TCPSegment) =
    if tcp_RST seg then []
    else let seg' = make_rst_segment_from_seg seg
          in [TCPMessage seg']

dropwithreset_ignore_or_fail = dropwithreset

tcp_close_temp sock =
    sock { cb = (cb sock) { cantrcvmore = True
                          , cantsndmore = True
                          , local_addr = TCPAddr (IPAddr 0,0)
                          , remote_addr = TCPAddr (IPAddr 0,0)
                          , bsd_cantconnect = True
                          }
         , st = CLOSED
         , cb_snd = (cb_snd sock) { sndq = bufferchain_empty }
         }


tcp_close :: SocketID -> HMonad t ()
tcp_close sid = 
  do b <- has_sock sid
     when b $ do
        sock <- lookup_sock sid
        let pending_tasks = waiting_list sock
            has_parent = (get_local_port $ parent_id $ cb sock) /= 0
        let result = map (\(_,cont) -> cont (SockError "tcpclose")) pending_tasks
        emit_ready result
        delete_sock sid
        when (not has_parent) $ free_local_port $ get_local_port sid

tcp_drop_and_close :: SocketID -> HMonad t ()
tcp_drop_and_close sid = 
  do b <- has_sock sid
     when b $ do
        sock <- lookup_sock sid
        let outsegs = if st sock `notElem` [CLOSED,LISTEN,SYN_SENT] 
                       then  [TCPMessage $ make_rst_segment_from_cb 
                              (sock) (local_addr $ cb sock) (remote_addr $ cb sock)]
                       else []
        emit_segs outsegs
        tcp_close sid

alloc_local_port :: HMonad t (Maybe Port)
alloc_local_port = do
  h <- get_host
  case local_ports h of
   [] -> return Nothing
   port:rest -> do put_host $ h { local_ports = rest }
                   return $ Just port

free_local_port port =
  modify_host $ \h -> h { local_ports = port:(local_ports h) }

