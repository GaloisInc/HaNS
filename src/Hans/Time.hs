{-# LANGUAGE RecordWildCards #-}

module Hans.Time (
    module Hans.Time,
    H.Entry(..)
  ) where

import qualified Data.Heap as H
import           Data.Time.Clock (UTCTime)


type Expires = H.Entry UTCTime

expiresBefore :: UTCTime -> Expires a -> Bool
expiresBefore time entry = time >= H.priority entry


type ExpireHeap a = H.Heap (Expires a)

emptyHeap :: ExpireHeap a
emptyHeap  = H.empty
{-# INLINE emptyHeap #-}

-- | Given the current time, partition the heap into entries that have expired,
-- and entries that remain valid.
partitionInvalid :: UTCTime -> ExpireHeap a -> (ExpireHeap a, ExpireHeap a)
partitionInvalid now heap = H.break (expiresBefore now) heap
{-# INLINE partitionInvalid #-}

-- | Add an entry to the 'ExpireHeap'.
addEntry :: UTCTime -> a -> ExpireHeap a -> ExpireHeap a
addEntry priority payload = H.insert H.Entry { .. }
{-# INLINE addEntry #-}

nullHeap :: ExpireHeap a -> Bool
nullHeap  = H.null
{-# INLINE nullHeap #-}
