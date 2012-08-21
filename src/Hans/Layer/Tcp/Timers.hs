module Hans.Layer.Tcp.Timers (
    initTimers
  , slowTimer
  , fastTimer

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
import Hans.Layer.Timer

import Control.Monad (when,guard)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Foldable as F


-- Timer Handlers --------------------------------------------------------------

-- | Schedule a @Tcp@ action to run every n milliseconds.
--
-- XXX we should investigate timing the time that body takes to run, and making
-- the decision about how long to delay based on that.
every :: Milliseconds -> Tcp () -> Tcp ()
every len body = do
  tcp    <- self
  timers <- timerHandle
  let loop = output $ delay timers len $ send tcp $ do
        body
        loop
  loop

-- | Schedule the timers to run on the fast and slow intervals.
initTimers :: Tcp ()
initTimers  = do
  every 500 slowTimer
  every 200 fastTimer

-- | Fires every 500ms.
slowTimer :: Tcp ()
slowTimer  = do
  eachConnection $ do
    handle2MSL
    handleRTO
    decrementTimers
    incIdle

  -- approximate rate of increase for the slow timer
  addInitialSeqNum 64000

-- | 75 second delay.
tcpKeepIntVal :: SlowTicks
tcpKeepIntVal  = 75 * 2

incIdle :: Sock ()
incIdle  = modifyTcpSocket_ (\tcp -> tcp { tcpIdle = tcpIdle tcp + 1 })

-- | Handle only the delayed ack timer, fires ever 200ms.
fastTimer :: Tcp ()
fastTimer  = eachConnection $ do
  tcp <- getTcpSocket
  guard (tcpNeedsDelAck tcp)
  ack


-- Timer Interaction -----------------------------------------------------------

-- | Decrement all non-zero timers by one tick.
decrementTimers :: Sock ()
decrementTimers  = modifyTcpSocket_ update
  where
  update tcp = tcp
    { tcpTimer2MSL = decrement tcpTimer2MSL
    }
    where
    decrement prj
      | val == 0  = 0
      | otherwise = val - 1
      where
      val = prj tcp

-- | Conditionally run a timer, when this slow-timer tick will decrement it to
-- zero.
whenTimer :: (TcpSocket -> SlowTicks) -> Sock () -> Sock ()
whenTimer prj body = do
  tcp <- getTcpSocket
  when (prj tcp == 1) body


-- 2MSL ------------------------------------------------------------------------

-- | Maximum segment lifetime, two minutes in this case.
mslTimeout :: SlowTicks
mslTimeout  = 2 * 60 * 2

-- | Set the value of the 2MSL timer.
set2MSL :: SlowTicks -> Sock ()
set2MSL val = modifyTcpSocket_ (\tcp -> tcp { tcpTimer2MSL = val })

-- | The timer that handles the TIME_WAIT, as well as the idle timeout.
handle2MSL :: Sock ()
handle2MSL  = whenTimer tcpTimer2MSL $ do
  tcp <- getTcpSocket
  if tcpState tcp /= TimeWait && tcpIdle tcp <= tcpMaxIdle tcp
     then set2MSL tcpKeepIntVal
     else closeSocket


-- RTO -------------------------------------------------------------------------

-- | Update segments in the outgoing window, decrementing their RTO timers, and
-- retransmitting them if it expires.
handleRTO :: Sock ()
handleRTO  = F.mapM_ outputSegment =<< modifyTcpSocket update
  where
  update tcp = (segs,tcp { tcpOut = win' })
    where
    (segs,win') = genRetransmitSegments (tcpOut tcp)

-- | Calibrate the RTO timer, as specified by RFC-6298.
calibrateRTO :: POSIXTime -> POSIXTime -> TcpSocket -> TcpSocket
calibrateRTO sent ackd tcp
  | tcpSRTT tcp == 0 = initial
  | otherwise        = rolling
  where

  -- round trip measurement
  r = ackd - sent

  -- no data has been sent before, seed the RTO values.
  initial = updateRTO tcp
    { tcpSRTT   = r
    , tcpRTTVar = r / 2
    }

  -- data has been sent, update based on previous values
  alpha   = 0.125
  beta    = 0.25
  rttvar  = (1 - beta)  * tcpRTTVar tcp + beta  * abs (tcpSRTT tcp * r)
  srtt    = (1 - alpha) * tcpSRTT   tcp + alpha * r
  rolling = updateRTO tcp
    { tcpRTTVar = rttvar
    , tcpSRTT   = srtt
    }

  -- update the RTO timer length
  updateRTO tcp' = tcp'
    { tcpRTO = ceiling (tcpSRTT tcp' + max 0.5 (2 * tcpRTTVar tcp'))
    }
