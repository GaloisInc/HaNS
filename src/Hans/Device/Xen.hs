module Hans.Device.Xen where

import Hans.Layer.Ethernet

import XenDevice.NIC as Xen
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

-- Xen NIC ---------------------------------------------------------------------

xenSend :: NIC -> L.ByteString -> IO ()
xenSend = Xen.sendPacket

xenReceiveLoop :: NIC -> EthernetHandle -> IO ()
xenReceiveLoop nic eh = Xen.setReceiveHandler nic k
  where
  k bs | L.length bs <= 14 = return ()
       | otherwise         = queueEthernet eh (S.concat (L.toChunks bs))
