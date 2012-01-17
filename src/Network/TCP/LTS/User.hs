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

module Network.TCP.LTS.User 
  ( tcp_process_user_request
  , tcp_wakeup
  )
where

import Foreign.C
import Data.List as List
import Control.Monad
import Data.Maybe
import Network.TCP.Type.Base
import Network.TCP.Type.Datagram
import Network.TCP.Type.Syscall
import Network.TCP.Type.Socket
import Hans.Layer.Tcp.Monad
import Network.TCP.Aux.SockMonad
import Network.TCP.Aux.Misc
import Network.TCP.Aux.Param
import Network.TCP.Aux.Output

import Network.TCP.LTS.Out

---------------------------------------------------------------------------------------
-- input:  a list of sock request
-- output: threads that have taken the completed requests (back in running state)
-- side effect: host state changes
--              blocked threads goes into the wait queue of each socket
-- tcp_process_user_requests :: (Monad m) => [(SockReq,SockRsp->t)] -> HMonad t m [t]
-- tcp_process_user_requests reqs = 
--  do r <- mapM tcp_process_user_request reqs
--    return $ concat r

tcp_process_user_request ::   (SockReq,SockRsp->t) -> HMonad t (Maybe t)
tcp_process_user_request (req, cont) =
 case req of
   SockListen addr   -> process_listen addr cont
   SockClose  sid    -> process_close sid cont
   SockConnect local addr  -> process_connect local addr cont
   SockAccept sid    -> process_accept sid cont
   SockSend sid d    -> process_send sid d cont
   SockRecv sid -> process_recv sid cont

tcp_wakeup_request req cont = 
 case req of 
   SockConnect local addr  -> wakeup_connect cont
   SockAccept sid    -> wakeup_accept sid cont
   SockSend sid d    -> wakeup_send sid d cont
   SockRecv sid -> wakeup_recv sid cont
  
-- pre-cond: sock is set
-- post-cond: sock is set, tcp_output needed
tcp_wakeup =
 do sock <- get_sock
    case waiting_list sock of
     [] -> return ()
     (req,cont):reqs -> do
        res <- tcp_wakeup_request req cont
        case res of
          Nothing -> return ()
          Just th -> do
           emit_ready_ [th]
           modify_sock $ \s -> s {waiting_list = reqs}

-- pre-cond: sock not set
-- post-cond: sock not set
process_listen ::  Port -> (SockRsp->t) -> HMonad t (Maybe t)
process_listen port cont =
 do let sock_id = SocketID (port, TCPAddr (IPAddr 0,0))
    h <- get_host
    -- check if port has been used...
    if port `elem` (local_ports h) then
       do let listen = SocketListen [] [] listen_qlimit
          let newsock = initial_tcp_socket 
                        { cb = (cb initial_tcp_socket) { local_addr = TCPAddr (IPAddr 0,port), self_id=sock_id }
                        , st = LISTEN
                        , sock_listen = listen
                        }
          insert_sock sock_id newsock
          modify_host $ \h -> h { local_ports = List.delete port (local_ports h) }          
          return $ Just $ cont $ SockNew sock_id
     else
       return $ Just $ cont $ SockError "Port not available"

process_close ::  SocketID -> (SockRsp->t) -> HMonad t (Maybe t)
process_close sid cont =
 do ok <- has_sock sid
    if not ok then 
       return $ Just $ cont $ SockError "Socket not found"
     else do
       sock <- lookup_sock sid
       if st sock `elem` [CLOSED,SYN_SENT,SYN_RECEIVED] then do
          -- close_7 : delete sid
          tcp_close sid
          return $ Just $ cont $ SockOK
        else if st sock /= LISTEN then runSMonad sid $ do
          -- close_1 : change the flags so FIN can be sent later
          modify_sock $ \sock-> sock { cb = (cb sock) { cantsndmore=True, cantrcvmore=True}
                                     , cb_rcv = (cb_rcv sock) { rcvq=bufferchain_empty }
                                     }
          tcp_output_all
          return $ Just $ cont $ SockOK
        else do
          -- close_8 : closing a LISTEN socket
          -- todo: not implemented yet
          return $ Just $ cont $ SockError "not implemented yet: close_8 : closing a LISTEN socket"

-- pre-cond: sock not set
-- post-cond: sock not set
process_accept ::  SocketID -> (SockRsp->t) -> HMonad t (Maybe t)
process_accept sid cont = 
 do ok <- has_sock sid
    if not ok then 
       return $ Just $ cont $ SockError "Socket not found"
     else runSMonad sid $ do
       res <- try_accept cont
       when (isNothing res) $
          -- put thread in waiting list
          modify_sock $ \sock -> sock {waiting_list = (waiting_list sock)++[(SockAccept sid, cont)] }
       return res
wakeup_accept sid cont
  = try_accept cont

-- pre-cond: sock is set
-- post-cond: sock is set, listening queue updated
try_accept ::  (SockRsp->t) -> SMonad t (Maybe t)
try_accept cont =
 do    sock <- get_sock
       if st sock /= LISTEN then 
          return $ Just $ cont $ SockError "Socket not in LISTEN state"
        else do
          -- find the listen queue
          let listen = sock_listen sock
          case lis_q listen of 
           []        -> return Nothing  -- no connection to accept, can't proceed
           (sid2:qs) -> do         -- try to accept sid2, either success or fail 
             modify_sock $ \sock -> sock { sock_listen = listen { lis_q = qs } }
             return $ Just $ cont $ SockNew sid2

process_recv ::  SocketID  -> (SockRsp->t) -> HMonad t (Maybe t)
process_recv sid cont =
 do ok <- has_sock sid
    if not ok then
       return $ Just $ cont $ SockError "Socket not found"
     else runSMonad sid $ do
       res <- try_recv cont
       when (isNothing res) $ 
          -- put thread in waiting list
          modify_sock $ \sock -> sock {waiting_list = (waiting_list sock)++[(SockRecv sid, cont)] }
       return res
   
wakeup_recv sid cont =
    try_recv cont

try_recv ::  (SockRsp->t) -> SMonad t (Maybe t)
try_recv cont = 
 do    sock <- get_sock
       let q = rcvq $ cb_rcv sock
       if st sock `elem` [ CLOSED, SYN_SENT, SYN_RECEIVED] then
          return $ Just $ cont $ SockError "Socket not in synchronized state"
        else if bufc_length q == 0 then
          if cantrcvmore $ cb sock 
           then return $ Just $ cont $ SockData buffer_empty -- EOF
           else return Nothing -- no data, can't proceed
        else do
          -- let rcvnum = min size (length q)
          -- (q1,q2) = splitAt rcvnum q
          put_sock $ sock { cb_rcv = (cb_rcv sock) {rcvq = bufferchain_tail q }}
          return $ Just $ cont $ SockData $ bufferchain_head q

process_send ::  SocketID -> Buffer -> (SockRsp->t) -> HMonad t (Maybe t)
process_send sid d cont = 
 do ok <- has_sock sid
    if not ok then
       return $ Just $ cont $ SockError "Socket not found"
     else runSMonad sid $ do
       (res,remain) <- try_send d cont
       when (isNothing res) $
          -- put thread in waiting list
          modify_sock $ \sock -> sock {waiting_list = (waiting_list sock)++[(SockSend sid remain, cont)] }
       return res
   
wakeup_send sid d cont =
 do (res,remain) <- try_send d cont
    when (isNothing res) $
       -- put thread in waiting list again
       modify_sock $ \sock -> sock {waiting_list = (tail $ waiting_list sock)++[(SockSend sid remain, cont)] }
    return res

try_send ::  Buffer -> (SockRsp->t) -> SMonad t (Maybe t, Buffer)
try_send d cont = 
 do    sock <- get_sock
       if st sock `notElem` [ ESTABLISHED, CLOSE_WAIT] then
          return (Just $ cont $ SockError "Socket not in synchronized state", buffer_empty )
        else if cantsndmore $ cb sock then
          return (Just $ cont $ SockError "Socket cantsndmore=true, cannot send...", buffer_empty)
        else do
          let max_can_send = freebsd_so_sndbuf - (bufc_length $ sndq $ cb_snd sock)
              num_to_send = min max_can_send (buf_len d)
              (d1,d2) = buffer_split num_to_send d
          modify_cb_snd $ \c -> c
             { sndq = sndq c `bufferchain_append` d1
             }
          --if (bufc_length (sndq $ cb_snd $ sock) == 0) then
          --    modify_cb_rcv $ \c -> c { tt_delack = True }
          -- else
          tcp_output_all
          if buf_len d2 == 0 then return (Just $ cont $ SockOK, d2)
                             else return (Nothing, d2)
         

process_connect :: IPAddr -> TCPAddr -> (SockRsp->t) -> HMonad t (Maybe t)
process_connect local addr cont = do
  h <- get_host
  m_port <- alloc_local_port
  if m_port == Nothing then return $ Just $ cont $ SockError "cannot allocate local port" else do
  let (Just port) = m_port
      sock_id = SocketID (port, addr)
      newiss = SeqLocal 1000 -- beginning iss. (todo: add more randomness)
      request_r_scale' = 0
      rcv_wnd' = freebsd_so_rcvbuf
      adv_mss = Just mssdflt
      tf_req_tstmp' = False -- todo: change it
      t_rttseg' = Just (ticks h, newiss)
  let { newsock = initial_tcp_socket 
        { st = SYN_SENT
        , cb_time = initial_cb_time
           { tt_conn_est = Just (create_timer (clock h) tcptv_keep_init)
           } 
        , cb_snd = initial_cb_snd
           { tt_rexmt = start_tt_rexmtsyn 0 False (t_rttinf initial_cb_snd) (clock h)
           , snd_una = newiss
           , snd_nxt = newiss `seq_plus` 1
           , snd_max = newiss `seq_plus` 1
           , t_rttseg = t_rttseg'
           }
        , cb_rcv = initial_cb_rcv
           { rcv_wnd = rcv_wnd'
           , rcv_adv = (rcv_nxt initial_cb_rcv) `seq_plus` rcv_wnd'
           , tf_rxwin0sent = (rcv_wnd' == 0)
           }   
        , cb = initial_cb_misc 
           { local_addr = TCPAddr (local,port)
           , remote_addr = addr
           , self_id=sock_id 
           , cantsndmore = False
           , cantrcvmore = False
           , iss = newiss
           , request_r_scale = Just request_r_scale'
           , t_advmss = adv_mss
           , tf_req_tstmp = tf_req_tstmp'
           }
        }}
  insert_sock sock_id newsock
  emit_segs $ [TCPMessage $ make_syn_segment (clock h) newsock (ticks h)]
  return $ Nothing

wakeup_connect ::  (SockRsp->t) -> SMonad t (Maybe t)
wakeup_connect cont = do    
  sock <- get_sock
  if st sock == SYN_SENT 
   then return Nothing 
   else return $ Just $ cont $ SockNew $ self_id $ cb sock
