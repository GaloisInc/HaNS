{-# LANGUAGE RecordWildCards #-}

module Hans.Ethernet (
    module Exports,
    module Hans.Ethernet
  ) where

import Hans.Device.Types
import Hans.Ethernet.Types as Exports
import Hans.Serialize (runPutPacket)

import           Control.Concurrent.BoundedChan (tryWriteChan)
import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as L


-- | Send a message out via a device.
sendEthernet :: Device -> Mac -> EtherType -> L.ByteString -> IO ()
sendEthernet Device { .. } eDest eType payload =
  do let packet = runPutPacket 14 100 payload
                $ putEthernetHeader EthernetHeader { eSource = devMac, .. }

     -- if the packet is too big for the device, throw it away
     if (fromIntegral (L.length packet) > dcMtu devConfig + 14)
        then updateError statTX devStats
        else do queued <- tryWriteChan devSendQueue packet
                unless queued (updateDropped statTX devStats)
