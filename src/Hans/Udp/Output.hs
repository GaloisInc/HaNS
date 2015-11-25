{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Udp.Output where

import Hans.Device.Types (Device(..),DeviceConfig(..))
import Hans.IP4.Output (primSendIP4)
import Hans.IP4.Packet (IP4,pattern IP4_PROT_UDP)
import Hans.Udp.Packet
import Hans.Types

import qualified Data.ByteString.Lazy as L


-- | Send Udp over IP4 with a pre-computed route.
primSendUdp4 :: NetworkStack
             -> Device
             -> IP4          -- ^ Source addr
             -> UdpPort      -- ^ Source port
             -> IP4          -- ^ Destination addr
             -> UdpPort      -- ^ Destination port
             -> IP4          -- ^ Next hop
             -> L.ByteString -- ^ Payload
             -> IO Bool

primSendUdp4 ns dev src udpSourcePort dst udpDestPort next payload
    -- XXX should this record an error?
  | L.length payload > 65527 = return False

  | otherwise =
    do let hdr   = UdpHeader { udpChecksum = 0, .. }
       let bytes = renderUdpPacket (not (dcChecksumOffload (devConfig dev)))
                       src dst hdr payload

       primSendIP4 ns dev src dst next IP4_PROT_UDP bytes

       return True
