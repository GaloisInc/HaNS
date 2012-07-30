module Hans.Layer.Tcp.Timers (
    initTimers
  , slowTimer
  , fastTimer
  ) where

import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Monad
import Hans.Layer.Timer


-- Timer Handlers --------------------------------------------------------------

every :: Milliseconds -> Tcp () -> Tcp ()
every len body = do
  tcp    <- self
  timers <- timerHandle
  output $ delay timers len $ send tcp $ do
    body
    every len body

initTimers :: Tcp ()
initTimers  = do
  every 500 slowTimer
  every 100 fastTimer

slowTimer :: Tcp ()
slowTimer  = do
  output (putStrLn "slow timer")

fastTimer :: Tcp ()
fastTimer  = do
  output (putStrLn "fast timer")


