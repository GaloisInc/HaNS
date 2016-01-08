module Hans.Threads where

import Control.Concurrent (forkIO,ThreadId)
import Control.Exception (catch,SomeException,fromException,AsyncException(..))


forkNamed :: String -> IO () -> IO ThreadId
forkNamed str body = forkIO (body `catch` showExn)
  where
  showExn :: SomeException -> IO ()
  showExn e =
    case fromException e of
      Just ThreadKilled -> return ()
      _                 -> putStrLn (str ++ ": Exception escaped: " ++ show e)
