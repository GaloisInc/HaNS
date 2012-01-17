{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Udp where

import Hans.Utils
import Hans.Utils.Checksum

import Control.Applicative ((<$>))
import Data.Serialize.Get (Get,getWord16be,isolate,label,getBytes)
import Data.Serialize.Put (Putter,runPut,putWord16be)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


-- Udp Ports -------------------------------------------------------------------

newtype UdpPort = UdpPort { getUdpPort :: Word16 }
  deriving (Eq,Ord,Num,Show,Enum,Bounded)

parseUdpPort :: Get UdpPort
parseUdpPort  = UdpPort <$> getWord16be

renderUdpPort :: Putter UdpPort
renderUdpPort  = putWord16be . getUdpPort


-- Udp Packets -----------------------------------------------------------------

data UdpHeader = UdpHeader
  { udpSourcePort :: !UdpPort
  , udpDestPort   :: !UdpPort
  , udpChecksum   :: !Word16
  } deriving Show

parseUdpPacket :: Get (UdpHeader,S.ByteString)
parseUdpPacket  = do
  src <- parseUdpPort
  dst <- parseUdpPort
  b16 <- getWord16be
  let len = fromIntegral b16
  label "UDPPacket" $ isolate (len - 6) $ do
    cs  <- getWord16be
    bs  <- getBytes (len - 8)
    let hdr = UdpHeader
          { udpSourcePort = src
          , udpDestPort   = dst
          , udpChecksum   = cs
          }
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
  hdrSize  = 8
  len      = fromIntegral (L.length body + hdrSize)
  ph       = mk len
  pcs      = computePartialChecksum 0 ph

  -- real header
  hdrBytes = runPut $ do
    renderUdpPort (udpSourcePort hdr)
    renderUdpPort (udpDestPort   hdr)
    putWord16be   (fromIntegral len)
    putWord16be 0 -- initial checksum

  -- body, and final checksum
  hcs = computePartialChecksum pcs hdrBytes
  cs  = finalizeChecksum (computePartialChecksumLazy hcs body)
