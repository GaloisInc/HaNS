{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Arp.Types where

import           Hans.Config (Config(..))
import           Hans.Device.Types (Device(..))
import           Hans.Ethernet.Types (Mac,getMac,putMac,pattern ETYPE_IPV4)
import           Hans.IP4.Types (IP4,getIP4,putIP4)
import qualified Hans.HashTable as HT
import           Hans.Time

import           Control.Concurrent (ThreadId,forkIO,threadDelay)
import           Control.Monad (guard,forever)
import qualified Data.Foldable as F
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import           Data.Serialize.Get (Get,getWord8,getWord16be,label)
import           Data.Serialize.Put (Putter,putWord16be,putWord8)
import           Data.Time.Clock
                     (NominalDiffTime,getCurrentTime,addUTCTime)
import           Data.Word (Word16)


-- Arp State -------------------------------------------------------------------

data ArpState = ArpState { arpTable :: !ArpTable
                           -- ^ Cached information

                         , arpAddrs :: !ArpAddrs
                           -- ^ Addresses held by this network stack

                         , arpPurgeThread :: !ThreadId
                           -- ^ The 'ThreadID' of the thread that periodically
                           -- purges the Arp table.
                         }

newArpState :: Config -> IO ArpState
newArpState cfg =
  do arpTable       <- newArpTable cfg
     arpAddrs       <- newIORef []
     arpPurgeThread <- forkIO (purgeArpTable cfg arpTable)
     return ArpState { .. }

type ArpAddrs = IORef [(IP4,Device)]

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


-- Arp Packets -----------------------------------------------------------------

-- | Arp packets, specialized to IP4 and Mac addresses.
data ArpPacket = ArpPacket { arpOper   :: {-# UNPACK #-} !ArpOper
                           , arpSHA    :: !Mac
                           , arpSPA    :: !IP4
                           , arpTHA    :: !Mac
                           , arpTPA    :: !IP4
                           } deriving (Show)

-- | Parse an Arp packet, given a way to parse hardware and protocol addresses.
getArpPacket :: Get ArpPacket
getArpPacket  = label "ArpPacket" $
  do hwtype <- getWord16be
     ptype  <- getWord16be
     hwlen  <- getWord8
     plen   <- getWord8

     -- make sure that this packet is specialized to IP4/Ethernet
     guard $ hwtype == 0x1        && hwlen == 6
          && ptype  == ETYPE_IPV4 && plen  == 4

     arpOper   <- getArpOper

     arpSHA    <- getMac
     arpSPA    <- getIP4

     arpTHA    <- getMac
     arpTPA    <- getIP4

     return ArpPacket { .. }

-- | Render an Arp packet, given a way to render hardware and protocol
-- addresses.
putArpPacket :: Putter ArpPacket
putArpPacket ArpPacket { .. } =
  do putWord16be   0x1
     putWord16be   ETYPE_IPV4
     putWord8      6
     putWord8      4

     putArpOper    arpOper

     putMac        arpSHA
     putIP4        arpSPA

     putMac        arpTHA
     putIP4        arpTPA


-- Arp Opcodes -----------------------------------------------------------------

type ArpOper = Word16

pattern ArpRequest = 0x1
pattern ArpReply   = 0x2

-- | Parse an Arp operation.
getArpOper :: Get ArpOper
getArpOper  =
  do w <- getWord16be
     guard (w == ArpRequest || w == ArpReply)
     return w
{-# INLINE getArpOper #-}

-- | Render an Arp operation.
putArpOper :: Putter ArpOper
putArpOper  = putWord16be
{-# INLINE putArpOper #-}
