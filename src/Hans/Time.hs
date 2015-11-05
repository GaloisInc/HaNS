{-# LANGUAGE RecordWildCards #-}

module Hans.Time (
    module Hans.Time,
    H.Entry(..)
  ) where

import qualified Data.Heap as H
import           Data.Time.Clock (UTCTime,NominalDiffTime,diffUTCTime)
import           Data.Tuple (swap)


type Expires = H.Entry UTCTime

expiresBefore :: UTCTime -> Expires a -> Bool
expiresBefore time entry = time >= H.priority entry


type ExpireHeap a = H.Heap (Expires a)

emptyHeap :: ExpireHeap a
emptyHeap  = H.empty
{-# INLINE emptyHeap #-}

-- | Given the current time, partition the heap into valid entries, and entries
-- that have expired.
partitionExpired :: UTCTime -> ExpireHeap a -> (ExpireHeap a, ExpireHeap a)
partitionExpired now heap = swap (H.break (expiresBefore now) heap)
{-# INLINE partitionExpired #-}

-- | Add an entry to the 'ExpireHeap', and return the time of the next
-- expiration event.
expireAt :: Eq a => UTCTime -> a -> ExpireHeap a -> (ExpireHeap a,UTCTime)
expireAt time a heap =
  let heap' = H.insert H.Entry { H.priority = time, H.payload = a }
            $ H.filter (\ H.Entry { .. } -> payload /= a ) heap
   in (heap',H.priority (H.minimum heap'))
   -- ^ NOTE: it's safe to use the partial function minimum, as we just inserted
   -- into the heap we're asking for the minimum element of.
{-# INLINE expireAt #-}

nullHeap :: ExpireHeap a -> Bool
nullHeap  = H.null
{-# INLINE nullHeap #-}

-- | The amount of time until the top of the heap expires, relative to the time
-- given.
expirationDelay :: UTCTime -> ExpireHeap a -> Maybe NominalDiffTime
expirationDelay now heap =
  do (H.Entry { .. }, _) <- H.viewMin heap
     return $! diffUTCTime priority now


-- | Convert a 'NominalDiffTime' into microseconds for use with 'threadDelay'.
toUSeconds :: NominalDiffTime -> Int
toUSeconds diff = truncate (diff * 1000000)
