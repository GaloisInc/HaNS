{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Udp.Packet where

import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.IP4.Packet (IP4,ip4PseudoHeader)
import Hans.Serialize

import qualified Data.ByteString.Lazy as L
import           Data.Serialize (Get,getWord16be,label,Putter,Put,putWord16be)
import           Data.Word (Word16)


-- Udp Ports -------------------------------------------------------------------

type UdpPort = Word16

getUdpPort :: Get UdpPort
getUdpPort  = getWord16be
{-# INLINE getUdpPort #-}

putUdpPort :: Putter UdpPort
putUdpPort  = putWord16be
{-# INLINE putUdpPort #-}


-- Udp Header ------------------------------------------------------------------

data UdpHeader = UdpHeader { udpSourcePort :: {-# UNPACK #-} !UdpPort
                           , udpDestPort   :: {-# UNPACK #-} !UdpPort
                           , udpChecksum   :: {-# UNPACK #-} !Word16
                           } deriving (Eq,Show)

emptyUdpHeader :: UdpHeader
emptyUdpHeader  = UdpHeader { udpSourcePort = 0
                            , udpDestPort   = 0
                            , udpChecksum   = 0 }

udpHeaderSize :: Int
udpHeaderSize  = 8

-- | Parse out a @UdpHeader@, and the size of the payload.
getUdpHeader :: Get (UdpHeader,Int)
getUdpHeader  = label "UDP Header" $
  do udpSourcePort <- getUdpPort
     udpDestPort   <- getUdpPort
     len           <- getWord16be
     udpChecksum   <- getWord16be
     return (UdpHeader { .. },fromIntegral len - udpHeaderSize)

-- | Render a @UdpHeader@.
putUdpHeader :: UdpHeader -> Int -> Put
putUdpHeader UdpHeader { .. } bodyLen =
  do putUdpPort  udpSourcePort
     putUdpPort  udpDestPort
     putWord16be (fromIntegral (bodyLen + udpHeaderSize))
     putWord16be 0
