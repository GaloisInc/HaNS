{-# LANGUAGE RecordWildCards #-}

module Hans.Queue (
    Queue()
  , newQueue
  , enqueue, tryEnqueue
  , dequeue, tryDequeue
  ) where

import Control.Concurrent.STM
import Control.Monad (guard)
import Data.Array.MArray


-- | Bounded queues.
data Queue a = Queue { queueLen  :: {-# UNPACK #-} !Int
                     , queueFree :: {-# UNPACK #-} !(TVar Int)
                     , queueHead :: {-# UNPACK #-} !(TVar Int)
                     , queueTail :: {-# UNPACK #-} !(TVar Int)
                     , queueData :: !(TArray Int a)
                     }

-- | Construct a new queue of the given length. Note that the length specifies
-- entries in the queue, and not total memory occupied by the queue.
newQueue :: Int -> IO (Queue a)
newQueue queueLen = atomically $
  do queueFree <- newTVar queueLen
     queueHead <- newTVar 0
     queueTail <- newTVar 0
     queueData <- newArray_ (0, queueLen - 1)
     return Queue { .. }

-- | Consume one free slot in the queue.
allocSlot :: TVar Int -> STM ()
allocSlot var =
  do free <- readTVar var
     guard (free > 0)
     modifyTVar' var (subtract 1)

-- | Free a slot in the queue.
freeSlot :: Int -> TVar Int -> STM ()
freeSlot len var =
  do free <- readTVar var
     guard (free < len)
     modifyTVar' var (+ 1)

-- | Increment a queue pointer, and return the last value.
movePointer :: Int -> TVar Int -> STM Int
movePointer len var =
  do ix <- readTVar var
     writeTVar var $! (ix + 1) `mod` len
     return ix

-- | Always succeeds, returning 'True' when the queue was written to.
tryEnqueue :: Queue a -> a -> STM Bool
tryEnqueue queue a = performWrite `orElse` return False
  where
  performWrite =
    do enqueue queue a
       return True

-- | Returns 'True' when the item was written to the queue, and 'False' if the
-- queue was full.
enqueue :: Queue a -> a -> STM ()
enqueue Queue { .. } a =
  do allocSlot queueFree
     ix <- movePointer queueLen queueHead
     writeArray queueData ix a

-- | Poll a queue for new data.
tryDequeue :: Queue a -> STM (Maybe a)
tryDequeue queue = readQueue `orElse` return Nothing
  where
  readQueue =
    do a <- dequeue queue
       return (Just a)

-- | Retries if the queue has no data to read.
dequeue :: Queue a -> STM a
dequeue Queue { .. } =
  do freeSlot queueLen queueFree
     ix <- movePointer queueLen queueTail
     readArray queueData ix
