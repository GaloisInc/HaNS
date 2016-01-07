{-# LANGUAGE RecordWildCards #-}

module Hans.Buffer.Stream where

import Control.Concurrent (MVar,newEmptyMVar)
import Data.IORef (IORef,newIORef)
import Data.Word (Word8)
import Foreign (ForeignPtr,mallocForeignPtrBytes)


data Buffer = Buffer { bufMem     :: !(ForeignPtr Word8)
                     , bufSize    :: !Int
                     , bufState   :: !(IORef State)
                     , bufWaiters :: !(MVar ())
                     }

data State = State { sReadHead  :: !Int
                   , sWriteHead :: !Int
                   , sAvail     :: !Int
                   }

emptyState :: Int -> State
emptyState sAvail = State { sReadHead  = 0
                          , sWriteHead = 0
                          , .. }

newBuffer :: Int -> IO Buffer
newBuffer bufSize =
  do bufMem     <- mallocForeignPtrBytes bufSize
     bufState   <- newIORef (emptyState bufSize)
     bufWaiters <- newEmptyMVar
     return Buffer { .. }
