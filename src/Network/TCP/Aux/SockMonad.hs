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

module Network.TCP.Aux.SockMonad where

import Hans.Layer.Tcp.Monad
import Network.TCP.Type.Base
import Network.TCP.Type.Datagram (IPMessage)
import Network.TCP.Type.Socket


data HState t = HState 
 { hs_host :: !(Host t)
 , hs_sock :: !(TCPSocket t)
 }

newtype SMonad t a = SMonad (HState t -> (a, HState t))

instance Monad (SMonad t) where
    return a = SMonad (\s -> (a,s))
    {-# INLINE return #-}

    x >>= f = bindSMonad x f
    {-# INLINE (>>=) #-}

bindSMonad :: SMonad t a -> (a -> SMonad t b) -> SMonad t b
bindSMonad (SMonad x) f =
  SMonad $ \s -> let (res, s')  = x s
                     (SMonad z) = f res in z s'
{-# INLINE bindSMonad #-}

get_host_ :: SMonad t (Host t)
get_host_  = SMonad (\s -> (hs_host s,s))
{-# INLINE get_host_ #-}

modify_host_ :: (Host t -> Host t) -> SMonad t ()
modify_host_ f = SMonad (\s -> ((), s { hs_host = f (hs_host s)}))
{-# INLINE modify_host_ #-}

emit_segs_ :: [IPMessage] -> SMonad t ()
emit_segs_ segs =
  modify_host_ (\ h -> h { output_queue = (output_queue h) ++ segs})
{-# INLINE emit_segs_ #-}

emit_ready_ :: [t] -> SMonad t ()
emit_ready_ threads =
  modify_host_ (\ h -> h { ready_list = (ready_list h)++ threads})
{-# INLINE emit_ready_ #-}

--------------------------------------------------
-- get_sid = do
--  SMonad $ \s -> (hs_sid s,s)
-- {-# INLINE get_sid #-}

get_sock :: SMonad t (TCPSocket t)
get_sock = SMonad (\s -> (hs_sock s,s))
{-# INLINE get_sock #-}

put_sock :: TCPSocket t -> SMonad t ()
put_sock sock = SMonad (\s -> ((), s { hs_sock = sock}))
{-# INLINE put_sock #-}

modify_sock :: (TCPSocket t -> TCPSocket t) -> SMonad t ()
modify_sock f = SMonad (\s -> ((), s { hs_sock = f (hs_sock s)}))
{-# INLINE modify_sock #-}

modify_cb :: (TCBMisc -> TCBMisc) -> SMonad t ()
modify_cb f = modify_sock (\sock -> sock { cb = f (cb sock) })
{-# INLINE modify_cb #-}

modify_cb_snd :: (TCBSending -> TCBSending) -> SMonad t ()
modify_cb_snd f = modify_sock (\sock -> sock { cb_snd = f (cb_snd sock) })
{-# INLINE modify_cb_snd #-}

modify_cb_rcv :: (TCBReceiving -> TCBReceiving) -> SMonad t ()
modify_cb_rcv f = modify_sock (\sock -> sock { cb_rcv = f (cb_rcv sock) })
{-# INLINE modify_cb_rcv #-}

modify_cb_time :: (TCBTiming -> TCBTiming) -> SMonad t ()
modify_cb_time f = modify_sock (\sock -> sock { cb_time = f (cb_time sock) })
{-# INLINE modify_cb_time #-}


-----------------------------------------------------
-- has_sock_ :: SocketID -> SMonad t Bool
-- has_sock_ sid = do 
--   h <- get_host_
--   return $ Map.member sid (sock_map h)
-- 
-- lookup_sock_ sid = do
--   h <- get_host_
--   res <- Map.lookup sid (sock_map h)
--   return res
-- 
-- {-# INLINE has_sock_ #-}
-- {-# INLINE lookup_sock_ #-}
-- 
------------------

runSMonad :: SocketID -> (SMonad t a) -> HMonad t a
runSMonad sid (SMonad m) = do
  h <- get_host
  sock <- lookup_sock sid
  let initstate = HState h sock 
  let (res, finalstate) = m initstate
  put_host $ hs_host finalstate
  update_sock sid $ \_ -> hs_sock finalstate
  return res
