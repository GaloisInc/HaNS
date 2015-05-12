{-# LANGUAGE RecordWildCards #-}

module Hans.Layer.Tcp.Timers (
    initTimers

  , resetIdle
  , whenIdleFor

  , set2MSL

  , calibrateRTO
  ) where

import Hans.Channel
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window

import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (when,unless,forever,void)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Foldable as F


-- Timer Handlers --------------------------------------------------------------

-- | Schedule the timers to run on the fast and slow intervals.
initTimers :: TcpHandle -> IO ()
initTimers tcp = do
  every 500 slowTimer
  every 200 fastTimer

  where

  -- XXX we should investigate timing the time that body takes to run, and
  -- making the decision about how long to delay based on that.
  every n body = void $ forkIO $ forever $
    do threadDelay timeout
       send tcp body
    where
    timeout = n * 1000

-- | Fires every 500ms.
slowTimer :: Tcp ()
slowTimer  =
  do -- first, handle active connections
     eachConnection $ do
       -- the slow timer is valid for all states but TimeWait; the check is done
       -- here because the socket may have only just transitioned to TimeWait,
       -- and hasn't been moved to hostTimeWaits yet.
       TcpSocket { .. } <- getTcpSocket
       unless (tcpState == TimeWait) handleRTO
       handle2MSL
       updateTimers

     -- second, decrement the 2MSL timer for sockets in the TimeWait state
     modifyHost_ $ \ host ->
       host { hostTimeWaits = stepTimeWaitConnections (hostTimeWaits host) }

resetIdle :: Sock ()
resetIdle  = modifyTcpTimers_ (\tt -> tt { ttIdle = 0 })

-- | Handle only the delayed ack timer, fires ever 200ms.
fastTimer :: Tcp ()
fastTimer  = eachConnection $ do
  tcp <- getTcpSocket
  when (tcpState tcp /= TimeWait && needsDelayedAck tcp) ack


-- Timer Interaction -----------------------------------------------------------

-- | Update timers, decrementing to 0 ones that expire, and incrementing the
-- others.
updateTimers :: Sock ()
updateTimers  =
  modifyTcpSocket_ $ \ tcp ->
    let tt = tcpTimers tcp
     in tcp { tcpTimers = tt
              { tt2MSL = decrement (tt2MSL tt)
              , ttIdle = increment (ttIdle tt)
              }
            }
  where
  decrement 0   = 0
  decrement val = pred val
  increment     = succ


-- | Conditionally run a timer, when this slow-timer tick will decrement it to
-- zero.
whenTimer :: (TcpTimers -> SlowTicks) -> Sock () -> Sock ()
whenTimer prj body = do
  tt <- getTcpTimers
  when (prj tt == 1) body

-- | Conditionally run an action when the connection has been idle for at least
-- timeout @SlowTick@s.
whenIdleFor :: Int -> Sock () -> Sock ()
whenIdleFor timeout body = do
  tt <- getTcpTimers
  when (ttIdle tt >= timeout) body


-- 2MSL ------------------------------------------------------------------------

-- | Set the value of the 2MSL timer.
set2MSL :: SlowTicks -> Sock ()
set2MSL val = modifyTcpTimers_ (\tt -> tt { tt2MSL = val })

-- | 75 second delay.
tcpKeepIntVal :: SlowTicks
tcpKeepIntVal  = 150

-- | The timer that handles the TIME_WAIT, as well as the idle timeout.
handle2MSL :: Sock ()
handle2MSL  =
  do whenTimer tt2MSL $
       do TcpSocket { .. } <- getTcpSocket
          let TcpTimers { .. } = tcpTimers
          if tcpState /= TimeWait && ttIdle <= ttMaxIdle
             then set2MSL tcpKeepIntVal
             else closeSocket

-- RTO -------------------------------------------------------------------------

-- | Update segments in the outgoing window, decrementing their RTO timers, and
-- retransmitting them if it expires.
handleRTO :: Sock ()
handleRTO  = do
  s <- getState
  when (s /= Closed) (F.mapM_ outputSegment =<< modifyTcpSocket update)
  where
  update tcp = (segs,tcp { tcpOut = win' })
    where
    (segs,win') = genRetransmitSegments (tcpOut tcp)

-- | Calibrate the RTO timer, as specified by RFC-6298.
calibrateRTO :: POSIXTime -> POSIXTime -> TcpTimers -> TcpTimers
calibrateRTO sent ackd tt
  | ttSRTT tt == 0 = initial
  | otherwise      = rolling
  where

  -- round trip measurement
  r = ackd - sent

  -- no data has been sent before, seed the RTO values.
  initial = updateRTO tt
    { ttSRTT   = r
    , ttRTTVar = r / 2
    }

  -- data has been sent, update based on previous values
  alpha   = 0.125
  beta    = 0.25
  rttvar  = (1 - beta)  * ttRTTVar tt + beta  * abs (ttSRTT tt * r)
  srtt    = (1 - alpha) * ttSRTT   tt + alpha * r
  rolling = updateRTO tt
    { ttRTTVar = rttvar
    , ttSRTT   = srtt
    }

  -- update the RTO timer length, bounding it at 64 seconds (128 ticks)
  updateRTO tt' = tt'
    { ttRTO = min 128 (ceiling (ttSRTT tt' + max 0.5 (2 * ttRTTVar tt')))
    }
