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

module Network.TCP.Pure
( module Network.TCP.Type.Syscall
, Host, IPMessage, Time
, tcp_init_host    -- :: Time -> [Port] -> Host t
, tcp_user_req     -- :: (SockReq, SockRsp -> t) -> Host t -> (Host t, Maybe t)
, tcp_user_rsp     -- :: Host t -> (Host t, [t])
, tcp_packet_in    -- :: IPMessage -> Host t -> Host t
, tcp_packet_out   -- :: Host t -> (Host t, [IPMessage])
, tcp_timer        -- :: Time -> Host t -> Host t
, tcp_timer_check  -- :: Time -> Host t -> IO (Host t) 
)
where

import Network.TCP.Type.Base
import Network.TCP.Type.Datagram
import Network.TCP.Type.Socket
import Network.TCP.Type.Syscall
-- import TCP.Impl.PacketIO

import Network.TCP.LTS.In
import Network.TCP.LTS.Out
import Network.TCP.LTS.Time
import Network.TCP.LTS.User

import Hans.Layer.Tcp.Monad

import Foreign.C
import Data.Map as Map

tcp_init_host :: Time -> [Port] -> Host t
tcp_init_host curr_time ports = Host 
      { output_queue = []
      , sock_map = Map.empty
      , clock = curr_time
      , ticks = Timestamp 0
      , next_timers = (curr_time + 200*1000,curr_time + 500*1000)
      , ready_list = []
      , local_ports = ports
      }

tcp_user_req :: (SockReq, SockRsp -> t) -> Host t -> (Host t, Maybe t)
tcp_user_req req h =
   runHMonad_ (tcp_process_user_request req) h

tcp_user_rsp :: Host t -> (Host t, [t])
tcp_user_rsp h = ( h { ready_list = [] }, ready_list h )

tcp_timer :: Time -> Host t -> Host t
tcp_timer tm = runHMonad $ do 
   h <- get_host
   let oldclock = clock h
       newclock = tm
       newtick = (ticks h) `seq_plus` (fromIntegral (newclock - oldclock) `div` 10000)
   put_host $ h { clock = newclock, ticks=newtick }
   tcp_update_timers

tcp_packet_in :: IPMessage -> Host t -> Host t
tcp_packet_in (TCPMessage seg) = 
   runHMonad $ tcp_deliver_in_packet seg

tcp_packet_out :: Host t -> (Host t, [IPMessage])
tcp_packet_out h = ( h { output_queue = [] }, output_queue h )

tcp_timer_check :: Time -> Host t -> IO (Host t) 
tcp_timer_check curr_time h = do
     let deadline = min (fst $ next_timers h) (snd $ next_timers h)
         error :: Integer = (fromIntegral curr_time) - (fromIntegral $ deadline)
         percent = fromInteger error / 100000.0 * 100
    
     if (curr_time > deadline +250*1000) then do
        putStrLn $ "Warning: too slow to maintain 200ms/500ms timer ticks, distance (should be within 200000us) = "++
                 (show $ curr_time - deadline) ++ "us"
        return $ h { next_timers= (curr_time,curr_time) }
      else return h

{-# INLINE  tcp_init_host     #-}
{-# INLINE  tcp_packet_in     #-}
{-# INLINE  tcp_packet_out    #-}
{-# INLINE  tcp_timer         #-}
{-# INLINE  tcp_user_req      #-}
{-# INLINE  tcp_user_rsp      #-}
{-# INLINE  tcp_timer_check   #-}
