{-# LANGUAGE BangPatterns #-}

module Hans.Layer.Tcp (
    TcpHandle
  , runTcpLayer
  , queueTcp
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Handlers
import Hans.Layer.Tcp.Monad
import Hans.Layer.Timer
import Hans.Message.Tcp
import Hans.Utils

import Control.Concurrent (forkIO)
import qualified Data.ByteString as S


runTcpLayer :: TcpHandle -> IP4Handle -> TimerHandle -> IO ()
runTcpLayer tcp ip4 t = do
  let s0 = emptyTcpState tcp ip4 t
  void (forkIO (loopLayer s0 (receive tcp) id))
  addIP4Handler ip4 tcpProtocol (queueTcp tcp)

-- | Queue a tcp packet.
queueTcp :: TcpHandle -> IP4 -> IP4 -> S.ByteString -> IO ()
queueTcp tcp !src !dst !bs = send tcp (handleIncomingTcp src dst bs)
