module Hans.Timers (
    Milliseconds
  , Timer()
  , delay
  , delay_
  , cancel
  ) where

import Control.Concurrent (forkIO,ThreadId,threadDelay,killThread)


type Milliseconds = Int

newtype Timer = Timer ThreadId
                deriving (Show,Eq)

-- | Delay an action, giving back a handle to allow the timer to be cancelled.
delay :: Milliseconds -> IO () -> IO Timer
delay n body = Timer `fmap` forkIO (threadDelay n >> body)

-- | Delay an action.
delay_ :: Milliseconds -> IO () -> IO ()
delay_ n body =
  do _ <- forkIO (threadDelay n >> body)
     return ()

-- | Cancel a delayed action.
cancel :: Timer -> IO ()
cancel (Timer tid) = killThread tid
