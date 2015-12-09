{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Udp.Output (
    primSendUdp
  ) where

import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Device.Types (ChecksumOffload(..),txOffload)
import Hans.Lens (view)
import Hans.Network
import Hans.Serialize (runPutPacket)
import Hans.Udp.Packet
import Hans.Types

import qualified Data.ByteString.Lazy as L
import           Data.Serialize (putWord16be)


-- | Send Udp over IP4 with a pre-computed route.
primSendUdp :: (HasNetworkStack ns, Network addr)
            => ns
            -> RouteInfo addr
            -> addr         -- ^ Destination addr
            -> UdpPort      -- ^ Source port
            -> UdpPort      -- ^ Destination port
            -> L.ByteString -- ^ Payload
            -> IO Bool

primSendUdp ns ri dst udpSourcePort udpDestPort payload
    -- XXX should this record an error?
  | L.length payload > 65527 = return False

  | otherwise =
    do let hdr   = UdpHeader { udpChecksum = 0, .. }
       let bytes = renderUdpPacket (view txOffload ri)
                       (riSource ri) dst hdr payload

       sendDatagram ns ri dst PROT_UDP bytes

       return True


-- | Given a way to make the pseudo header, render the UDP packet.
renderUdpPacket :: Network addr
                => ChecksumOffload -> addr -> addr -> UdpHeader -> L.ByteString
                -> L.ByteString
renderUdpPacket ChecksumOffload { .. } src dst hdr body
  | coUdp     = bytes
  | otherwise = beforeCS `L.append` withCS

  where

  pktlen = fromIntegral (L.length body)
  bytes  = runPutPacket udpHeaderSize 0 body (putUdpHeader hdr pktlen)

  udplen = udpHeaderSize + pktlen
  cs     = finalizeChecksum
         $ extendChecksum bytes
         $ pseudoHeader src dst PROT_UDP udplen

  beforeCS = L.take (fromIntegral udpHeaderSize - 2) bytes
  afterCS  = L.drop (fromIntegral udpHeaderSize    ) bytes

  withCS   = runPutPacket 2 0 afterCS (putWord16be cs)
