{-# LANGUAGE RecordWildCards #-}

module Hans.HashTable (
    HashTable(),
    newHashTable,
    insert,
    lookup,
    delete,
    mapHashTable,
    filterHashTable,
    alter,
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

mapBuckets :: (Eq k, Hashable k) => ([(k,a)] -> [(k,a)]) -> HashTable k a -> IO ()
mapBuckets f HashTable { .. } = go 0
  where
  update bucket = (f bucket, ())

  go ix | ix < htSize = do atomicModifyIORef' (htBuckets ! ix) update
                           go (succ ix)
        | otherwise   = return ()
{-# INLINE mapBuckets #-}


filterHashTable :: (Eq k, Hashable k)
                => (k -> a -> Bool) -> HashTable k a -> IO ()
filterHashTable p = mapBuckets (filter (uncurry p))
{-# INLINE filterHashTable #-}

mapHashTable :: (Eq k, Hashable k) => (k -> a -> a) -> HashTable k a -> IO ()
mapHashTable f = mapBuckets (map f')
  where
  f' (k,a) = (k, f k a)
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


-- | Create, update, or delete an entry in the hash table, returning some
-- additional value derived from the operation.
-- NOTE: This operation is only useful when you need to perform any of these
-- operations at the same time -- the specialized versions of the individual
-- behaviors all perform slightly better.
alter :: (Eq k, Hashable k)
       => (Maybe a -> (Maybe a, b)) -> k -> HashTable k a -> IO b
alter f k ht = modifyBucket ht k (update id)
  where

  update mkBucket (e@(k',a):rest)
    | k == k'   = finish mkBucket (Just a) rest
    | otherwise = update (\l -> mkBucket ( e : l )) rest

  update mkBucket [] = finish mkBucket Nothing []

  finish mkBucket mb rest =
    case f mb of
      (Just a, b) -> (mkBucket ((k,a):rest), b)
      (Nothing,b) -> (mkBucket        rest , b)
{-# INLINE alter #-}
