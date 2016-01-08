module Hans.Buffer.Signal where

import Control.Concurrent (MVar,newEmptyMVar,tryPutMVar,takeMVar,tryTakeMVar)

type Signal = MVar ()

newSignal :: IO Signal
newSignal  = newEmptyMVar

signal :: Signal -> IO ()
signal var =
  do _ <- tryPutMVar var ()
     return ()

waitSignal :: Signal -> IO ()
waitSignal  = takeMVar

tryWaitSignal :: Signal -> IO Bool
tryWaitSignal var =
  do mb <- tryTakeMVar var
     case mb of
       Just () -> return True
       Nothing -> return False
