{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Network where

import           Hans.Addr (NetworkAddr,Addr)
import           Hans.Checksum (PartialChecksum)
import           Hans.Device.Types (Device,HasDeviceConfig(..))
import qualified Hans.IP4        as IP4
import qualified Hans.IP4.Packet as IP4
import qualified Hans.IP4.State  as IP4
import           Hans.Lens
import           Hans.Types

import qualified Data.ByteString.Lazy as L
import           Data.Typeable (Typeable)
import           Data.Word (Word8)


type Protocol = Word8


-- | Information about how to reach a specific destination address (source and
-- next-hop addresses, and device to use).
data RouteInfo addr = RouteInfo { riSource
                                  -- ^ The source address to use when sending
                                , riNext :: !addr
                                  -- ^ The next-hop in the route
                                , riDev :: !Device
                                  -- ^ The device used for delivery
                                }

instance HasDeviceConfig (RouteInfo addr) where
  deviceConfig = to riDev . deviceConfig


-- | Interaction with routing and message delivery for a network layer.
class NetworkAddr addr => Network addr where
  -- | Calculate the pseudo-header for checksumming a packet at this layer of
  -- the network.
  pseudoHeader :: addr -> addr -> Protocol -> Int -> PartialChecksum

  -- | Lookup a route to reach this destination address.
  lookupRoute :: HasNetworkStack ns => ns -> addr -> IO (Maybe (RouteInfo addr))

  -- | Send a single datagram to a destination.
  sendDatagram :: HasNetworkStack ns
               => ns -> RouteInfo addr -> addr -> Protocol -> L.ByteString
               -> IO ()


-- | Send a datagram and lookup routing information at the same time. Returns
-- 'False' if no route to the destination was known.
routeDatagram :: (HasNetworkStack ns, Network addr)
              => ns -> addr -> Protocol -> L.ByteString -> IO Bool
routeDatagram ns dst prot bytes =
  do mbRoute <- lookupRoute ns dst
     case mbRoute of
       Just route -> do sendDatagram ns route dst prot bytes
                        return True

       Nothing    -> return False


instance Network IP4.IP4 where
  pseudoHeader = IP4.ip4PseudoHeader

  lookupRoute ns ip4 =
    do mb <- IP4.lookupRoute4 (view networkStack ns) ip4
       case mb of
         Just (riSource,riNext,riDev) -> return $! Just RouteInfo { .. }
         Nothing                      -> return Nothing


  sendDatagram ns RouteInfo { .. } dst =
    IP4.primSendIP4 (view networkStack ns) riDev riSource dst riNext
