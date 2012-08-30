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
import Hans.Layer.Timer
import Hans.Message.Ip4
import Hans.Message.Tcp
import Hans.Utils

import Control.Concurrent (forkIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as S


runTcpLayer :: TcpHandle -> IP4Handle -> TimerHandle -> IO ()
runTcpLayer tcp ip4 t = do
  start <- getPOSIXTime
  let s0 = emptyTcpState tcp ip4 t start
  void (forkIO (loopLayer "tcp" s0 (receive tcp) stepTcp))
  addIP4Handler ip4 tcpProtocol (queueTcp tcp)

  -- initialize the timers
  send tcp initTimers

-- | Queue a tcp packet.
queueTcp :: TcpHandle -> IP4Header -> S.ByteString -> IO ()
queueTcp tcp !hdr !bs = send tcp (handleIncomingTcp hdr bs)

-- | Run the tcp action, after updating any internal state.
stepTcp :: Tcp () -> Tcp ()
stepTcp body = do
  now <- time
  modifyHost $ \ host ->
    let diff = now - hostLastUpdate host
        -- increment the ISN at 128KHz
        inc  = truncate (128000 * diff)
     in if inc > 0
           then host
             { hostLastUpdate    = now
             , hostInitialSeqNum = hostInitialSeqNum host + inc
             }
           else host
  body
