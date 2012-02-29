module Hans.Device.Xen where

import Hans.Layer.Ethernet
import Hans.Utils

import Data.Maybe (listToMaybe)
import XenDevice.NIC as Xen
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L


-- Utilities -------------------------------------------------------------------

infixl 1 >>=?
(>>=?) :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
m >>=? f = do
  mb <- m
  case mb of
    Nothing -> return Nothing
    Just a  -> f a

returnJust :: Monad m => a -> m (Maybe a)
returnJust  = return . Just


-- Xen NIC ---------------------------------------------------------------------


openXenDevice :: String -> IO (Maybe NIC)
openXenDevice _ =
  listToMaybe `fmap` Xen.potentialNICs >>=? \ dev ->
  initializeNIC dev Nothing


xenSend :: NIC -> L.ByteString -> IO ()
xenSend nic bs = void (Xen.transmitPacket nic bs)


xenReceiveLoop :: NIC -> EthernetHandle -> IO ()
xenReceiveLoop nic eh = Xen.setReceiveHandler nic k
  where
  k bs | L.length bs <= 14 = return ()
       | otherwise         = queueEthernet eh (S.concat (L.toChunks bs))
