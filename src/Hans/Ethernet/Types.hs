{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Ethernet.Types (
    -- * Ethernet Headers
    EthernetHeader(..), getEthernetHeader, putEthernetHeader,
    EtherType,

    -- ** MAC addresses
    Mac(..), getMac, putMac,
    pattern BroadcastMac,

    -- ** EtherType Patterns
    pattern ETYPE_IPV4,
    pattern ETYPE_ARP,
    pattern ETYPE_IPV6

  ) where

import           Data.Serialize
                     (Get,getWord8,getWord16be,Putter,putWord16be,putWord8)
import           Data.Word (Word8,Word16)


-- Mac Addresses ---------------------------------------------------------------

data Mac = Mac {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8
               deriving (Eq,Ord,Show)

getMac :: Get Mac
getMac  =
  do a <- getWord8
     b <- getWord8
     c <- getWord8
     d <- getWord8
     e <- getWord8
     f <- getWord8
     return $! Mac a b c d e f

putMac :: Putter Mac
putMac (Mac a b c d e f) =
  do putWord8 a
     putWord8 b
     putWord8 c
     putWord8 d
     putWord8 e
     putWord8 f

-- | The broadcast MAC address.
pattern BroadcastMac = Mac 0xff 0xff 0xff 0xff 0xff 0xff


-- Ethernet Headers ------------------------------------------------------------

type EtherType = Word16

data EthernetHeader = EthernetHeader
  { eDest    :: {-# UNPACK #-} !Mac
  , eSource  :: {-# UNPACK #-} !Mac
  , eType    :: {-# UNPACK #-} !EtherType
  } deriving (Eq,Show)

getEthernetHeader :: Get EthernetHeader
getEthernetHeader =
  do eDest   <- getMac
     eSource <- getMac
     eType   <- getWord16be
     return EthernetHeader { .. }

putEthernetHeader :: Putter EthernetHeader
putEthernetHeader EthernetHeader { .. } =
  do putMac      eDest
     putMac      eSource
     putWord16be eType


-- Common Ether-Types ----------------------------------------------------------

pattern ETYPE_IPV4 = 0x0800
pattern ETYPE_ARP  = 0x0806
pattern ETYPE_IPV6 = 0x86DD
