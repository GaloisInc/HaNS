{-# LANGUAGE PatternSynonyms #-}

module Hans.Tcp.Output (
    -- * Output
    routeTcp4,
    sendTcp4

    -- $notes
  ) where

import Hans.Device.Types (Device(..),DeviceConfig(..))
import Hans.IP4 (IP4,primSendIP4,pattern IP4_PROT_TCP,SendSource(..),sendIP4)
import Hans.Tcp.Packet (TcpHeader,renderTcpPacket4)
import Hans.Types

import qualified Data.ByteString.Lazy as L
import           Data.Word (Word32)


-- | Send outgoing tcp segments, with a route calculation.
--
-- See note "No Retransmit Queue" ("Hans.Tcp.Output#no-retransmit-queue").
routeTcp4 :: NetworkStack -> Device
          -> IP4 -> IP4 -> TcpHeader -> L.ByteString -> IO Bool
routeTcp4 ns dev src dst hdr payload
  | L.length payload > fromIntegral (maxBound :: Word32) =
    return False

  | otherwise =
    do let bytes = renderTcpPacket4 (not (dcChecksumOffload (devConfig dev)))
                       src dst hdr payload
       sendIP4 ns (SourceDev dev src) dst IP4_PROT_TCP bytes


-- | Lowest-level output function for TCP.
--
-- See note "No Retransmit Queue" ("Hans.Tcp.Output#no-retransmit-queue").
sendTcp4 :: NetworkStack -> Device
         -> IP4 -> IP4 -> IP4 -> TcpHeader -> L.ByteString -> IO Bool
sendTcp4 ns dev src dst next hdr payload
  | L.length payload >= fromIntegral (maxBound :: Word32) =
    return False

  | otherwise =
    do let bytes = renderTcpPacket4 (not (dcChecksumOffload (devConfig dev)))
                       src dst hdr payload
       primSendIP4 ns dev src dst next IP4_PROT_TCP bytes

       return True


-- $notes
-- #no-retransmit-queue#
--
-- = No Retransmit Queue
-- This function will not record entries in the retransmit queue, and is
-- responsible only for output to a lower layer.
