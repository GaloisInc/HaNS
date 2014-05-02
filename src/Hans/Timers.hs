module Hans.Timers (
    Milliseconds
  , Timer()
  , delay
  , delay_
  , cancel
  , expired
  ) where

import Control.Concurrent (forkIO,ThreadId,threadDelay,killThread
                          ,mkWeakThreadId)
import GHC.Conc (threadStatus,ThreadStatus(..))
import System.Mem.Weak (Weak,deRefWeak)


type Milliseconds = Int

-- | A handle to a scheduled timer.
--
-- NOTE: This keeps a weak reference to the thread containing the timer, to
-- allow it to still receive exceptions (see mkWeakThreadId).
newtype Timer = Timer (Weak ThreadId)

-- | Delay an action, giving back a handle to allow the timer to be cancelled.
delay :: Milliseconds -> IO () -> IO Timer
delay n body =
  do tid <- forkIO (threadDelay (n * 1000) >> body)
     wid <- mkWeakThreadId tid
     return (Timer wid)

-- | Delay an action.
delay_ :: Milliseconds -> IO () -> IO ()
delay_ n body =
  do _ <- forkIO (threadDelay (n * 1000) >> body)
     return ()

-- | Cancel a delayed action.
cancel :: Timer -> IO ()
cancel (Timer wid) =
  do mb <- deRefWeak wid
     case mb of
       Just tid -> killThread tid
       Nothing  -> return ()

expired :: Timer -> IO Bool
expired (Timer wid) =
  do mb <- deRefWeak wid
     case mb of
       Just tid -> do status <- threadStatus tid
                      case status of
                        ThreadRunning   -> return False
                        ThreadBlocked _ -> return False
                        _               -> return True

       Nothing  -> return True
