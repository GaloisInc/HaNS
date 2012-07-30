module Hans.Layer.Tcp.Timers (
    initTimers
  , slowTimer
  , fastTimer
  ) where

import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Timer

import Control.Monad (guard)


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
  output (putStrLn "slow timer")

-- | Handle only the delayed ack timer.
fastTimer :: Tcp ()
fastTimer  = eachConnection $ do
  tcp <- getTcpSocket
  guard (tcpNeedsDelAck tcp)
  ack
