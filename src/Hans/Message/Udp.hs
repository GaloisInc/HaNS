{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Udp where

import Hans.Utils
import Hans.Utils.Checksum

import Control.Applicative ((<$>))
import Data.Serialize.Get (Get,getWord16be,getByteString,isolate,label)
import Data.Serialize.Put (Putter,runPut,putWord16be,putByteString)
import Data.Word (Word16)
import qualified Data.ByteString as S


-- Udp Ports -------------------------------------------------------------------

newtype UdpPort = UdpPort { getUdpPort :: Word16 }
  deriving (Eq,Ord,Num,Show,Enum,Bounded)

parseUdpPort :: Get UdpPort
parseUdpPort  = UdpPort <$> getWord16be

renderUdpPort :: Putter UdpPort
renderUdpPort  = putWord16be . getUdpPort


-- Udp Packets -----------------------------------------------------------------

data UdpPacket = UdpPacket
  { udpHeader  :: !UdpHeader
  , udpPayload :: S.ByteString
  } deriving Show

data UdpHeader = UdpHeader
  { udpSourcePort :: !UdpPort
  , udpDestPort   :: !UdpPort
  , udpChecksum   :: !Word16
  } deriving Show

parseUdpPacket :: Get UdpPacket
parseUdpPacket  = do
  src <- parseUdpPort
  dst <- parseUdpPort
  b16 <- getWord16be
  let len = fromIntegral b16
  label "UDPPacket" $ isolate (len - 6) $ do
    cs  <- getWord16be
    bs  <- getByteString (len - 8)
    let hdr = UdpHeader
          { udpSourcePort = src
          , udpDestPort   = dst
          , udpChecksum   = cs
          }
    return $! UdpPacket hdr bs

-- | Given a way to make the pseudo header, render the UDP packet.
renderUdpPacket :: UdpPacket -> MkPseudoHeader -> IO Packet
renderUdpPacket (UdpPacket hdr bs) mk = pokeChecksum cs bytes 6
  where
  hdrSize = 8
  len     = S.length bs + hdrSize
  ph      = mk len
  pcs     = computePartialChecksum 0 ph
  bytes   = runPut $ do
    renderUdpPort (udpSourcePort hdr)
    renderUdpPort (udpDestPort   hdr)
    putWord16be   (fromIntegral len)
    putWord16be 0 -- initial checksum
    putByteString bs
  -- the checksum is 6 bytes into the rendered packet
  cs = computeChecksum pcs bytes
