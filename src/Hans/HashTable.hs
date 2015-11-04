{-# LANGUAGE RecordWildCards #-}

module Hans.HashTable (
    HashTable(),
    newHashTable,
    insert,
    lookup,
    delete,
    mapHashTable
  ) where

import           Prelude hiding (lookup)

import           Control.Monad (replicateM)
import           Data.Array (Array,listArray,(!))
import           Data.Hashable (Hashable,hash)
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import qualified Data.List as List


data HashTable k a =
  HashTable { htSize    :: {-# UNPACK #-} !Int
            , htBuckets :: {-# UNPACK #-} !(Array Int (Bucket k a))
            }

type Bucket k a = IORef [(k,a)]


-- | Create a new hash table with the given size.
newHashTable :: (Eq k, Hashable k) => Int -> IO (HashTable k a)
newHashTable htSize =
  do buckets <- replicateM htSize (newIORef [])
     return HashTable { htBuckets = listArray (0,htSize - 1) buckets
                      , .. }

mapHashTable :: (Eq k, Hashable k) => (k -> a -> a) -> HashTable k a -> IO ()
mapHashTable f HashTable { .. } = go 0
  where
  f' (k,a) = (k, f k a)

  update bucket = (map f' bucket, ())

  go ix | ix < htSize = do atomicModifyIORef' (htBuckets ! ix) update
                           go (succ ix)
        | otherwise   = return ()
{-# INLINE mapHashTable #-}

getBucket :: Hashable k => HashTable k a -> k -> Bucket k a
getBucket HashTable { .. } k = htBuckets ! key
  where
  key = hash k `mod` htSize
{-# INLINE getBucket #-}

modifyBucket :: Hashable k
             => HashTable k a -> k -> ([(k,a)] -> ([(k,a)],b)) -> IO b
modifyBucket ht k = atomicModifyIORef' (getBucket ht k)
{-# INLINE modifyBucket #-}

insert :: (Eq k, Hashable k) => k -> a -> HashTable k a -> IO ()
insert k a ht = modifyBucket ht k (\ bucket -> (addEntry bucket, ()))
  where
  addEntry (entry @ (k',_) : rest)
    | k' == k   = (k,a) : rest
    | otherwise = let rest' = addEntry rest
                   in rest' `seq` (entry : rest')

  addEntry []   = []
{-# INLINE insert #-}

lookup :: (Eq k, Hashable k) => k -> HashTable k a -> IO (Maybe a)
lookup k ht =
  do bucket <- readIORef (getBucket ht k)
     return $! List.lookup k bucket
{-# INLINE lookup #-}

delete :: (Eq k, Hashable k) => k -> HashTable k a -> IO ()
delete k ht = modifyBucket ht k (\ bucket -> (removeEntry bucket, ()))

  where

  removeEntry (entry @ (k',_):rest)
    | k' == k   = rest
    | otherwise = let rest' = removeEntry rest
                   in rest' `seq` (entry : rest')

  removeEntry []  = []
{-# INLINE delete #-}
