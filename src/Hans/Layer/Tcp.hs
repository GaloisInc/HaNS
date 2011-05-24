{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Layer.Tcp (
    TcpHandle
  , runTcpLayer
  , queueTcp

  , module Exports
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Monad (Tcp,TcpHandle,TcpState(..),emptyTcpState)
import Hans.Layer.Tcp.Socket as Exports
import Hans.Layer.Timer (TimerHandle,udelay)
import Hans.Message.Tcp (tcpProtocol)
import Hans.Layer.Tcp.Handlers (handleIncomingTcp,handleOutgoing)
import Hans.Utils (void)

import Network.TCP.Type.Base (posixtime_to_time)
import Network.TCP.Type.Socket (update_host_time)

import Control.Concurrent (forkIO)
import MonadLib (get,set)
import qualified Data.ByteString as S


runTcpLayer :: TcpHandle -> IP4Handle -> TimerHandle -> IO ()
runTcpLayer tcp ip4 t = do
  let s0 = emptyTcpState tcp ip4 t
  void (forkIO (loopLayer s0 (receive tcp) updateTimeAndRun))
  addIP4Handler ip4 tcpProtocol (queueTcp tcp)

-- | Queue a tcp packet.
queueTcp :: TcpHandle -> IP4 -> IP4 -> S.ByteString -> IO ()
queueTcp tcp !src !dst !bs = send tcp (handleIncomingTcp src dst bs)

-- | Pull the time out of the Layer monad, and convert it to a value that can be
-- used with the TCP layer.
updateTimeAndRun :: Tcp () -> Tcp ()
updateTimeAndRun body = do
  now <- time
  s   <- get
  set $! s { tcpHost = update_host_time (posixtime_to_time now) (tcpHost s) }
  body
  handleOutgoing
