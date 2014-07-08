{-# LANGUAGE RecordWildCards #-}

module Hans.Layer.Tcp.Timers (
    initTimers
  , slowTimer
  , fastTimer

  , resetIdle
  , whenIdleFor

  , mslTimeout
  , set2MSL

  , calibrateRTO
  ) where

import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Timers (Milliseconds)

import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (when,unless,guard,forever,void)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Foldable as F


-- Timer Handlers --------------------------------------------------------------

-- | Schedule a @Tcp@ action to run every n milliseconds.
--
-- XXX we should investigate timing the time that body takes to run, and making
-- the decision about how long to delay based on that.
every :: Milliseconds -> Tcp () -> Tcp ()
every len body = do
  tcp <- self
  let timeout = len * 1000
  output $ void $ forkIO $ forever $
    do threadDelay timeout
       send tcp body

-- | Schedule the timers to run on the fast and slow intervals.
initTimers :: Tcp ()
initTimers  = do
  every 500 slowTimer
  every 200 fastTimer

-- | Fires every 500ms.
slowTimer :: Tcp ()
slowTimer  = eachConnection $ do
  handle2MSL

  -- the slow timer is valid for all states but TimeWait
  TcpSocket { .. } <- getTcpSocket
  unless (tcpState == TimeWait) $
    do handleRTO
       handleFinWait2
       updateTimers

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

-- | Maximum segment lifetime, two minutes in this case.
mslTimeout :: SlowTicks
mslTimeout  = 2 * 60 * 2

-- | Set the value of the 2MSL timer.
set2MSL :: SlowTicks -> Sock ()
set2MSL val = modifyTcpTimers_ (\tt -> tt { tt2MSL = val })

-- | 75 second delay.
tcpKeepIntVal :: SlowTicks
tcpKeepIntVal  = 150

-- | The timer that handles the TIME_WAIT, as well as the idle timeout.
handle2MSL :: Sock ()
handle2MSL  = whenTimer tt2MSL $ do
  tcp <- getTcpSocket
  let tt = tcpTimers tcp
  if tcpState tcp /= TimeWait && ttIdle tt <= ttMaxIdle tt
     then set2MSL tcpKeepIntVal
     else closeSocket


-- FIN_WAIT_2 ------------------------------------------------------------------

-- | The FinWait2 10m timeout.
finWait2Idle :: SlowTicks
finWait2Idle  = 1200

-- | GC the connection if it's been open for a long time, in FIN_WAIT_2.
--
-- XXX not sure if this is the correct way to do this.  should this set a flag
-- to indicate that if the 2MSL timer goes off, the connection should be cleaned
-- up?
handleFinWait2 :: Sock ()
handleFinWait2  = whenState FinWait2
                $ whenIdleFor finWait2Idle
                $ set2MSL tcpKeepIntVal


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
