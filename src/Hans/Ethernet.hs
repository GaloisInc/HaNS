{-# LANGUAGE RecordWildCards #-}

module Hans.Ethernet (
    module Exports,
    module Hans.Ethernet
  ) where

import Hans.Device.Types
import Hans.Ethernet.Types as Exports
import Hans.Monad (Hans,decode',io,escape)
import Hans.Serialize (runPutPacket)

import           Control.Concurrent.BoundedChan (tryWriteChan)
import           Control.Monad (when,unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- | Decode an ethernet frame, or fail trying.
decodeEthernet :: InputPacket -> Hans (DeviceStats,EthernetHeader,S.ByteString)
decodeEthernet InputPacket { .. } =
  do let stats = devStats ipDevice
     (hdr,payload) <- decode' stats getEthernetHeader ipBytes
     return (stats,hdr,payload)


-- | Send a message out via a device.
sendEthernet :: Device -> Mac -> EtherType -> L.ByteString -> IO ()
sendEthernet Device { .. } eDest eType payload =
  do let packet = runPutPacket 14 100 payload
                $ putEthernetHeader EthernetHeader { eSource = devMac, .. }

     -- if the packet is too big for the device, throw it away
     if (fromIntegral (L.length packet) > dcMtu devConfig)
        then updateError devStats
        else do queued <- tryWriteChan devSendQueue packet
                unless queued (updateDropped devStats)
