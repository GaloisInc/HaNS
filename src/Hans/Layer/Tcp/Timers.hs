module Hans.Layer.Tcp.Timers (
    initTimers
  , slowTimer
  , fastTimer

  , mslTimeout
  , set2MSL
  ) where

import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Timer

import Control.Monad (when,guard)


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

slowTimer :: Tcp ()
slowTimer  = do
  eachConnection $ do
    handle2MSL
    decrementTimers
    incIdle

  -- approximate rate of increase for the slow timer
  addInitialSeqNum 64000

-- | 75 second delay.
tcpKeepIntVal :: SlowTicks
tcpKeepIntVal  = 75 * 2

-- | The timer that handles the TIME_WAIT, as well as the idle timeout.
handle2MSL :: Sock ()
handle2MSL  = whenTimer tcpTimer2MSL $ do
  outputS (putStrLn "2MSL")
  tcp <- getTcpSocket
  if tcpState tcp /= TimeWait && tcpIdle tcp <= tcpMaxIdle tcp
     then set2MSL tcpKeepIntVal
     else do
       outputS (putStrLn "closing socket!")
       setState Closed

incIdle :: Sock ()
incIdle  = modifyTcpSocket_ (\tcp -> tcp { tcpIdle = tcpIdle tcp + 1 })

-- | Handle only the delayed ack timer.
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

-- | Maximum segment lifetime, two minutes in this case.
mslTimeout :: SlowTicks
mslTimeout  = 8 --2 * 60 * 2

-- | Set the value of the 2MSL timer.
set2MSL :: SlowTicks -> Sock ()
set2MSL val = modifyTcpSocket_ (\tcp -> tcp { tcpTimer2MSL = val })
