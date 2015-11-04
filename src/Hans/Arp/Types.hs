{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Arp.Types where

import           Hans.Config (Config(..))
import           Hans.Ethernet.Types (Mac,getMac,putMac,pattern ETYPE_IPV4)
import           Hans.IP4.Types (IP4,getIP4,putIP4)
import qualified Hans.HashTable as HT
import           Hans.Time

import           Control.Monad (guard,unless)
import qualified Data.Foldable as F
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import qualified Data.Map.Strict as Map
import           Data.Serialize.Get (Get,getWord8,getWord16be,label)
import           Data.Serialize.Put (Putter,putWord16be,putWord8)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word16)


-- Arp State -------------------------------------------------------------------

data ArpState = ArpState { arpTable :: !ArpTable
                           -- ^ Cached information

                         , arpAddrs :: !ArpAddrs
                           -- ^ Addresses held by this network stack

                         }

newArpState :: Config -> IO ArpState
newArpState cfg =
  do arpTable <- newArpTable cfg
     arpAddrs <- newIORef Map.empty
     return ArpState { .. }

type ArpAddrs = IORef (Map.Map IP4 Mac)

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

-- | Expire entries in the 'ArpTable'.
expireEntries :: UTCTime -> ArpTable -> IO ()
expireEntries now ArpTable { .. } =
  do expired <- atomicModifyIORef' atExpire $ \ heap ->
                    let (expired,rest) = partitionInvalid now heap
                     in (rest,expired)

     unless (nullHeap expired)
       $ F.forM_ expired
       $ \ Entry { .. } -> HT.delete payload atMacs


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
getArpOper  = getWord16be
{-# INLINE getArpOper #-}

-- | Render an Arp operation.
putArpOper :: Putter ArpOper
putArpOper  = putWord16be
{-# INLINE putArpOper #-}
