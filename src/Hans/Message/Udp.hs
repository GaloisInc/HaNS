{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Udp where

import Hans.Address.IP4
import Hans.Message.Ip4
import Hans.Utils
import Hans.Utils.Checksum

import Control.Applicative ((<$>))
import Data.Serialize.Get (Get,getWord16be,isolate,label,getBytes,remaining)
import Data.Serialize.Put (Put,Putter,runPut,putWord16be)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


-- Udp Protocol Number ---------------------------------------------------------

udpProtocol :: IP4Protocol
udpProtocol  = IP4Protocol 0x11


-- Udp Ports -------------------------------------------------------------------

newtype UdpPort = UdpPort { getUdpPort :: Word16 }
  deriving (Eq,Ord,Num,Read,Show,Enum,Bounded)

parseUdpPort :: Get UdpPort
parseUdpPort  = UdpPort <$> getWord16be

renderUdpPort :: Putter UdpPort
renderUdpPort  = putWord16be . getUdpPort


-- Udp Header ------------------------------------------------------------------

data UdpHeader = UdpHeader
  { udpSourcePort :: !UdpPort
  , udpDestPort   :: !UdpPort
  , udpChecksum   :: !Word16
  } deriving Show

udpHeaderSize :: Int
udpHeaderSize  = 8

-- | Parse out a @UdpHeader@.
parseUdpHeader :: Get (UdpHeader,Int)
parseUdpHeader  = do
  src <- parseUdpPort
  dst <- parseUdpPort
  len <- getWord16be
  cs  <- getWord16be
  let hdr = UdpHeader src dst cs
  return (hdr,fromIntegral len)

-- | Render a @UdpHeader@.
renderUdpHeader :: UdpHeader -> Int -> Put
renderUdpHeader hdr bodyLen = do
  renderUdpPort (udpSourcePort hdr)
  renderUdpPort (udpDestPort hdr)
  putWord16be   (fromIntegral (bodyLen + udpHeaderSize))
  putWord16be   (udpChecksum hdr)


-- Udp Packets -----------------------------------------------------------------

parseUdpPacket :: Get (UdpHeader,S.ByteString)
parseUdpPacket  = do
  (hdr,len) <- parseUdpHeader
  label "UDPPacket" $ isolate (len - udpHeaderSize) $ do
    bs <- getBytes =<< remaining
    return (hdr,bs)

-- | Given a way to make the pseudo header, render the UDP packet.
renderUdpPacket :: UdpHeader -> L.ByteString -> MkPseudoHeader
                -> IO L.ByteString
renderUdpPacket hdr body mk = do
  -- the checksum is 6 bytes into the rendered packet
  hdrBytes' <- pokeChecksum cs hdrBytes 6
  return (L.fromChunks [hdrBytes'] `L.append` body)
  where
  -- pseudo header
  bodyLen  = fromIntegral (L.length body)
  ph       = mk (bodyLen + udpHeaderSize)
  pcs      = computePartialChecksum emptyPartialChecksum ph

  -- real header
  hdrBytes = runPut (renderUdpHeader (hdr { udpChecksum = 0 }) bodyLen)

  -- body, and final checksum
  hcs = computePartialChecksum pcs hdrBytes
  cs  = finalizeChecksum (computePartialChecksumLazy hcs body)

-- | Recreate the UDP checksum, given a rendered packet, and the source and
-- destination.
validateUdpChecksum :: IP4 -> IP4 -> S.ByteString -> Bool
validateUdpChecksum src dst bytes =
  finalizeChecksum (computePartialChecksum phcs bytes) == 0
  where
  phcs = computePartialChecksum emptyPartialChecksum
       $ mkIP4PseudoHeader src dst udpProtocol
       $ S.length bytes
