{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans      #-}

module Hans.Layer.Timer (
    TimerHandle
  , runTimerLayer

  , Milliseconds
  , delay
  , Microseconds
  , udelay
  ) where

import Hans.Channel

import Control.Concurrent (forkIO,threadDelay,MVar,newMVar,takeMVar,putMVar)
import Data.FingerTree as FT
import Data.Monoid (Monoid(..))
import Data.Time.Clock.POSIX (POSIXTime,getPOSIXTime)
import MonadLib


-- The Timers Structure --------------------------------------------------------

data Timer a = Timer
  { timerAt    :: POSIXTime
  , timerValue :: a
  }

instance Monoid POSIXTime where
  mempty = 0
  mappend a b | a > b     = a
              | otherwise = b

instance Measured POSIXTime (Timer a) where
  measure t = timerAt t

type Timers a = FingerTree POSIXTime (Timer a)


-- | Add an action to happen sometime in the future.
at :: POSIXTime -> a -> Timers a -> Timers a
at fut a ts = as' >< bs
  where
  (as,bs) = runnable fut ts
  as' = as |> Timer fut a


-- | Partition the actions into runnable and deferred.
runnable :: POSIXTime -> Timers a -> (Timers a, Timers a)
runnable now ts = FT.split (> now) ts


-- | Step through the timers in a group.
stepTimers :: Timers a -> Maybe (a, Timers a)
stepTimers ts = case viewl ts of
  EmptyL  -> Nothing
  r :< rs -> Just (timerValue r,rs)


-- | Run all timers, when the action is an IO action.
runTimers :: Timers (IO a) -> IO (Timers (IO a))
runTimers ts = do
  now <- getPOSIXTime
  let loop (Just (a,as)) = a >> loop (stepTimers as)
      loop Nothing       = return ()
      (rs,rest) = runnable now ts
  loop (stepTimers rs)
  return rest


-- The External Message Interface ----------------------------------------------

type ActionTimers = Timers (IO ())

-- | Mesages handled by the timer thread.
data TimerMessage = AddTimer POSIXTime (IO ())

-- | A channel to the timer thread.
type TimerHandle = Channel TimerMessage


-- | Start the timer thread.
runTimerLayer :: TimerHandle -> IO ()
runTimerLayer h = do
  timers <- newMVar FT.empty
  _      <- forkIO (actionHandler timers)
  _      <- forkIO (messageHandler h timers)
  return ()


type Milliseconds = Int

-- | Add a message to happen after some number of milliseconds.
delay :: TimerHandle -> Milliseconds -> IO () -> IO ()
delay h !off = udelay h (off * 1000)

type Microseconds = Int

-- | Add a message to happen after some number of microseconds.
udelay :: TimerHandle -> Microseconds -> IO () -> IO ()
udelay h !micros k = do
  now <- getPOSIXTime
  -- the granularity for a NominalTimeDiff is 10^-12
  let off = fromIntegral micros / 1000000
  send h (AddTimer (now + off) k)


-- Internal Loops --------------------------------------------------------------

-- | The delay granularity of the timer layer.
timerStep :: Microseconds
timerStep  = 100


-- | Loop, running available timer actions.
actionHandler :: MVar ActionTimers -> IO ()
actionHandler timers = forever $ do
  threadDelay timerStep
  putMVar timers =<< runTimers =<< takeMVar timers


-- | Loop, processing timer add requests.
messageHandler :: TimerHandle -> MVar ActionTimers -> IO ()
messageHandler h timers = forever $ do
  AddTimer t k <- receive h
  ts           <- takeMVar timers
  putMVar timers $! at t k ts
