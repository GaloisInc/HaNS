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
import Hans.Layer.Tcp.Types
import Hans.Message.Ip4
import Hans.Message.Tcp
import Hans.Utils

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Data.Time.Clock.POSIX (getPOSIXTime,POSIXTime)
import qualified Data.ByteString as S


runTcpLayer :: TcpHandle -> IP4Handle -> IO ()
runTcpLayer tcp ip4 = do
  start <- getPOSIXTime
  let s0 = emptyTcpState tcp ip4 start
  void (forkIO (loopLayer "tcp" s0 (receive tcp) stepTcp))
  addIP4Handler ip4 tcpProtocol (queueTcp tcp)

  -- initialize the timers
  initTimers tcp

-- | Queue a tcp packet.
queueTcp :: TcpHandle -> IP4Header -> S.ByteString -> IO ()
queueTcp tcp !hdr !bs = send tcp (handleIncomingTcp hdr bs)

-- | Rate of ISN increase, in Hz.
isnRate :: POSIXTime
isnRate  = 128000

-- | Run the tcp action, after updating any internal state.
stepTcp :: Tcp () -> Tcp ()
stepTcp body = do
  now        <- time
  lastUpdate <- getLastUpdate
  let diff = now - lastUpdate
      -- increment the ISN at 128KHz
      inc  = round (isnRate * diff)

  when (inc > 0) $
    modifyHost_ $ \ host ->
      host { hostLastUpdate    = now
           , hostInitialSeqNum = hostInitialSeqNum host + inc
           }

  body
