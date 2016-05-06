{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Udp.Output (
    primSendUdp,

    -- ** Fast-path Output
    responder,
    queueUdp,
  ) where

import Hans.Addr.Types (Addr)
import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Device.Types (ChecksumOffload(..),txOffload)
import Hans.Lens (view)
import Hans.Network
import Hans.Serialize (runPutPacket)
import Hans.Udp.Packet
import Hans.Types

import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import           Data.Serialize (putWord16be)


-- Fast-path Output ------------------------------------------------------------

responder :: NetworkStack -> IO ()
responder ns = forever $
  do msg <- BC.readChan chan
     case msg of
       SendDatagram ri dst hdr body ->
         do _ <- sendUdp ns ri dst hdr body
            return ()

  where
  chan = view udpQueue ns

queueUdp :: NetworkStack -> RouteInfo Addr -> Addr -> UdpHeader -> L.ByteString
         -> IO Bool
queueUdp ns ri dst hdr body
    -- XXX should this record an error?
  | L.length body > 65527 = return False
  | otherwise             = BC.tryWriteChan (view udpQueue ns)
                         $! SendDatagram ri dst hdr body


-- Output ----------------------------------------------------------------------

-- | Send Udp over IP4 with a pre-computed route.
primSendUdp :: Network addr
            => NetworkStack
            -> RouteInfo addr
            -> addr         -- ^ Destination addr
            -> UdpPort      -- ^ Source port
            -> UdpPort      -- ^ Destination port
            -> L.ByteString -- ^ Payload
            -> IO Bool
primSendUdp ns ri dst udpSourcePort udpDestPort payload
    -- XXX should this record an error?
  | L.length payload > 65527 = return False
  | otherwise                = sendUdp ns ri dst UdpHeader { udpChecksum = 0, .. } payload


sendUdp :: Network addr
        => NetworkStack
        -> RouteInfo addr -> addr
        -> UdpHeader -> L.ByteString
        -> IO Bool
sendUdp ns ri dst hdr payload =
  do let bytes = renderUdpPacket (view txOffload ri)
                     (riSource ri) dst hdr payload

     sendDatagram ns ri dst False PROT_UDP bytes

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
