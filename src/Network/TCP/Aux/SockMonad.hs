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

import Network.TCP.Type.Base
import Network.TCP.Type.Socket
import Network.TCP.Aux.Misc
import Hans.Layer.Tcp.Monad
import Control.Exception

data HState t = HState 
 { hs_host :: !(Host t)
 , hs_sock :: !(TCPSocket t)
 } 

newtype SMonad t a = SMonad (HState t -> (a, HState t))

instance Monad (SMonad t) where
    return a = SMonad $ \s -> (a,s)
    x >>= f = bindSMonad x f
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}

bindSMonad :: SMonad t a -> (a -> SMonad t b) -> SMonad t b
bindSMonad (SMonad x) f = 
        SMonad $ \s -> let (res, s') = x s
                           (SMonad z) = f res in z s'
{-# INLINE bindSMonad #-}

get_host_ :: SMonad t (Host t)
get_host_   = SMonad $ \s -> (hs_host s,s)

modify_host_ f = SMonad $ \s -> ((), s { hs_host = f (hs_host s)})
emit_segs_  segs = modify_host_ $ \h -> h { output_queue = (output_queue h)++ segs}
emit_ready_ threads = modify_host_ $ \h -> h { ready_list = (ready_list h)++ threads}

{-# INLINE get_host_ #-}
{-# INLINE modify_host_ #-}
{-# INLINE emit_segs_ #-}
{-# INLINE emit_ready_ #-}

--------------------------------------------------
-- get_sid = do
--  SMonad $ \s -> (hs_sid s,s)
-- {-# INLINE get_sid #-}

get_sock = do
  SMonad $ \s -> (hs_sock s,s)

put_sock sock = do
  SMonad $ \s -> ((), s { hs_sock = sock})

modify_sock f = do
  SMonad $ \s -> ((), s { hs_sock = f (hs_sock s)})
modify_cb     f =
  SMonad $ \s-> let sock=hs_sock s in ((), s { hs_sock=sock { cb    =f (cb     sock) }})
modify_cb_snd f =
  SMonad $ \s-> let sock=hs_sock s in ((), s { hs_sock=sock { cb_snd=f (cb_snd sock) }})
modify_cb_rcv f =
  SMonad $ \s-> let sock=hs_sock s in ((), s { hs_sock=sock { cb_rcv=f (cb_rcv sock) }})
modify_cb_time f=
  SMonad $ \s-> let sock=hs_sock s in ((), s { hs_sock=sock {cb_time=f (cb_time sock)}})
{-# INLINE get_sock #-}
{-# INLINE put_sock #-}
{-# INLINE modify_sock #-}
{-# INLINE modify_cb #-}
{-# INLINE modify_cb_snd #-}
{-# INLINE modify_cb_rcv #-}
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
