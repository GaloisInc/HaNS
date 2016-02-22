module Hans.Threads where

import Control.Concurrent (forkFinally,ThreadId)
import Control.Exception (fromException,AsyncException(..))


forkNamed :: String -> IO () -> IO ThreadId
forkNamed str body = forkFinally body showExn
  where

  showExn Right{} =
    return ()

  showExn (Left e) =
    case fromException e of
      Just ThreadKilled -> return ()
      _                 -> putStrLn (str ++ ": Exception escaped: " ++ show e)
