{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.UDP.Packet where

import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.IP4.Packet (IP4,ip4PseudoHeader,pattern IP4_PROT_UDP)
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


-- Udp Packets -----------------------------------------------------------------

-- | Given a way to make the pseudo header, render the UDP packet.
renderUdpPacket :: Bool -> IP4 -> IP4 -> UdpHeader -> L.ByteString
                -> L.ByteString
renderUdpPacket includeCS src dst hdr body
  | not includeCS = bytes
  | otherwise     = beforeCS `L.append` withCS

  where

  pktlen = fromIntegral (L.length body)
  bytes  = runPutPacket udpHeaderSize 0 body (putUdpHeader hdr pktlen)

  udplen = udpHeaderSize + pktlen
  cs     = finalizeChecksum
         $ extendChecksum bytes
         $ ip4PseudoHeader src dst IP4_PROT_UDP udplen

  beforeCS = L.take (fromIntegral udpHeaderSize - 2) bytes
  afterCS  = L.drop (fromIntegral udpHeaderSize    ) bytes

  withCS   = runPutPacket 2 0 afterCS (putWord16be cs)
