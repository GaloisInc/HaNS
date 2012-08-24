{-# LANGUAGE BangPatterns #-}

module Hans.Layer.Tcp (
    TcpHandle
  , runTcpLayer
  , queueTcp
  ) where

import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Handlers
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Timers
import Hans.Layer.Timer
import Hans.Message.Ip4
import Hans.Message.Tcp
import Hans.Utils

import Control.Concurrent (forkIO)
import qualified Data.ByteString as S


runTcpLayer :: TcpHandle -> IP4Handle -> TimerHandle -> IO ()
runTcpLayer tcp ip4 t = do
  let s0 = emptyTcpState tcp ip4 t
  void (forkIO (loopLayer s0 (receive tcp) id))
  addIP4Handler ip4 tcpProtocol (queueTcp tcp)

  -- initialize the timers
  send tcp initTimers

-- | Queue a tcp packet.
queueTcp :: TcpHandle -> IP4Header -> S.ByteString -> IO ()
queueTcp tcp !hdr !bs = send tcp (handleIncomingTcp hdr bs)
