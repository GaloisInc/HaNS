{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Udp.Input (
    processUdp4
  ) where

import qualified Hans.Buffer.Datagram as DG
import           Hans.Checksum (finalizeChecksum,extendChecksum)
import           Hans.Device (Device(..),DeviceConfig(..))
import           Hans.IP4.Packet (IP4,ip4PseudoHeader,pattern IP4_PROT_UDP)
import           Hans.Monad (Hans,decode',dropPacket,io)
import           Hans.Udp.Packet
import           Hans.Types

import           Control.Monad (unless)
import qualified Data.ByteString as S


processUdp4 :: NetworkStack -> Device -> IP4 -> IP4 -> S.ByteString -> Hans ()
processUdp4 ns dev src dst bytes =
  do let checksum = finalizeChecksum $ extendChecksum bytes
                                     $ ip4PseudoHeader src dst IP4_PROT_UDP
                                     $ S.length bytes

     unless (dcChecksumOffload (devConfig dev) || checksum == 0)
         (dropPacket (devStats dev))

     ((hdr,payloadLen),payload) <- decode' (devStats dev) getUdpHeader bytes

     -- attempt to find a destination for this packet
     io (routeMsg ns dev src dst hdr (S.take payloadLen payload))

routeMsg :: NetworkStack -> Device -> IP4 -> IP4 -> UdpHeader -> S.ByteString -> IO ()
routeMsg ns dev src dst UdpHeader { .. } payload =
  do mb <- lookupRecv4 ns dst udpDestPort
     case mb of

       -- XXX: which stat should increment when writeChunk fails?
       Just (Receiver4 buf) ->
         do _ <- DG.writeChunk buf (dev,src,udpSourcePort,dst,udpDestPort) payload
            return ()

       _ -> return ()
