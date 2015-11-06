{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.ArpTable where

import           Hans.Config (Config(..))
import           Hans.Ethernet (Mac)
import qualified Hans.HashTable as HT
import           Hans.IP4.Packet (IP4)
import           Hans.Time
                     (ExpireHeap,expireAt,emptyHeap,expirationDelay
                     ,toUSeconds,partitionExpired,Entry(..))

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import qualified Data.Foldable as F
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import           Data.Time.Clock (NominalDiffTime,addUTCTime,getCurrentTime)


-- | The Arp table consists of a map of IP4 to Mac, as well as a heap that
-- orders the IP4 addresses according to when their corresponding entries should
-- be expired.
--
-- INVARIANT: there should never be entries in the map that aren't also in the
-- heap.
data ArpTable = ArpTable { atMacs   :: !(HT.HashTable IP4 Mac)
                         , atExpire :: !(IORef (ExpireHeap IP4))
                         }

newArpTable :: Config -> IO ArpTable
newArpTable Config { .. } =
  do atMacs   <- HT.newHashTable cfgArpTableSize
     atExpire <- newIORef emptyHeap
     return ArpTable { .. }

-- | Loops forever, delaying until the next arp table entry needs to be purged.
-- If no entries exist, it waits for the maximum entry lifetime before checking
-- again.
purgeArpTable :: Config -> ArpTable -> IO ()
purgeArpTable Config { .. } table = forever $
  do mbDelay <- expireEntries table

     -- delay until the top of the heap expires, or the default entry lifetime
     -- if the heap is empty
     threadDelay (maybe defaultDelay toUSeconds mbDelay)

  where

  defaultDelay = toUSeconds cfgArpTableLifetime

-- | Expire entries in the 'ArpTable', and return the delay until the next entry
-- expires.
expireEntries :: ArpTable -> IO (Maybe NominalDiffTime)
expireEntries ArpTable { .. } =
  do now <- getCurrentTime

     -- partition the expiration heap by the current time
     (valid,expired) <- atomicModifyIORef' atExpire $ \ heap ->
                          let heaps = partitionExpired now heap
                           in (fst heaps, heaps)

     -- remove expired entries from the hash table
     F.mapM_ (\ Entry { .. } -> HT.delete payload atMacs) expired

     return $! expirationDelay now valid

-- | Lookup an entry in the Arp table.
lookupEntry :: IP4 -> ArpTable -> IO (Maybe Mac)
lookupEntry spa ArpTable { .. } = HT.lookup spa atMacs

-- | Insert an entry into the Arp table.
addEntry :: IP4 -> Mac -> NominalDiffTime -> ArpTable -> IO ()
addEntry spa sha lifetime ArpTable { .. } =
  do now <- getCurrentTime
     let end = addUTCTime lifetime now

     HT.insert spa sha atMacs

     -- XXX given that entries are expired in a separate thread, is the deadline
     -- actually necessary?
     _deadline <- atomicModifyIORef' atExpire (expireAt end spa)

     return ()


