{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Udp.Input (
    processUdp
  ) where

import           Hans.Addr (IP6,IP4,toIP6,fromIP6)
import qualified Hans.Buffer.Datagram as DG
import           Hans.Checksum (finalizeChecksum,extendChecksum)
import           Hans.Device (Device(..),ChecksumOffload(..),rxOffload)
import           Hans.Lens (view)
import           Hans.Monad (Hans,decode',dropPacket,io)
import           Hans.Nat.Forward (tryForwardUdp)
import           Hans.Network
import           Hans.Udp.Output (queueUdp)
import           Hans.Udp.Packet
import           Hans.Types

import           Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


{-# SPECIALIZE processUdp :: NetworkStack -> Device -> IP4 -> IP4
                          -> S.ByteString -> Hans Bool #-}

-- | Process a message destined for the UDP layer. When the message cannot be
-- routed, 'False' is returned.
processUdp :: Network addr
           => NetworkStack -> Device -> addr -> addr -> S.ByteString -> Hans Bool
processUdp ns dev src dst bytes =
  do let checksum = finalizeChecksum $ extendChecksum bytes
                                     $ pseudoHeader src dst PROT_UDP
                                     $ S.length bytes

     unless (coUdp (view rxOffload dev) || checksum == 0)
         (dropPacket (devStats dev))

     ((hdr,payloadLen),payload) <- decode' (devStats dev) getUdpHeader bytes

     let local  = toIP6 dst
         remote = toIP6 src

     -- attempt to find a destination for this packet
     io (routeMsg ns dev local remote hdr (S.take payloadLen payload))

routeMsg :: NetworkStack -> Device -> IP6 -> IP6 -> UdpHeader -> S.ByteString -> IO Bool
routeMsg ns dev local remote hdr payload =
  do mb <- lookupRecv ns remote (udpDestPort hdr)
     case mb of

       -- XXX: which stat should increment when writeChunk fails?
       Just buf ->
         do _ <- DG.writeChunk buf (dev,remote,udpSourcePort hdr,local,udpDestPort hdr) payload
            return True

       -- Check to see if there's a forwarding rule to use
       Nothing | Just local' <- fromIP6 local, Just remote' <- fromIP6 remote ->
         do mbFwd <- tryForwardUdp ns local' remote' hdr
            case mbFwd of
              Just (ri,dst',hdr') -> queueUdp ns (fmap toIP6 ri) (toIP6 dst') hdr' (L.fromStrict payload)
              Nothing             -> return False

       _ -> return False
