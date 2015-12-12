{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Network (
    module Hans.Network,
    module Hans.Network.Types
  ) where

import           Hans.Addr (NetworkAddr)
import           Hans.Addr.Types (Addr(..))
import           Hans.Checksum (PartialChecksum)
import           Hans.Device.Types (Device)
import qualified Hans.IP4        as IP4
import qualified Hans.IP4.State  as IP4
import           Hans.Lens
import           Hans.Network.Types
import           Hans.Types

import qualified Data.ByteString.Lazy as L


-- | Interaction with routing and message delivery for a network layer.
class NetworkAddr addr => Network addr where
  -- | Calculate the pseudo-header for checksumming a packet at this layer of
  -- the network.
  pseudoHeader :: addr -> addr -> NetworkProtocol -> Int -> PartialChecksum

  -- | Lookup a route to reach this destination address.
  lookupRoute :: HasNetworkStack ns => ns -> addr -> IO (Maybe (RouteInfo addr))

  -- | Send a single datagram to a destination.
  sendDatagram' :: HasNetworkStack ns
                => ns
                -> Device
                -> addr -- ^ Source
                -> addr -- ^ Destination
                -> addr -- ^ Next-hop
                -> NetworkProtocol
                -> L.ByteString
                -> IO ()


sendDatagram :: (HasNetworkStack ns, Network addr)
             => ns -> RouteInfo addr -> addr -> NetworkProtocol -> L.ByteString
             -> IO ()
sendDatagram ns RouteInfo { .. } = \ dst ->
  sendDatagram' ns riDev riSource dst riNext
{-# INLINE sendDatagram #-}



-- | Send a datagram and lookup routing information at the same time. Returns
-- 'False' if no route to the destination was known.
routeDatagram :: (HasNetworkStack ns, Network addr)
              => ns -> addr -> NetworkProtocol -> L.ByteString -> IO Bool
routeDatagram ns dst prot bytes =
  do mbRoute <- lookupRoute ns dst
     case mbRoute of
       Just route -> do sendDatagram ns route dst prot bytes
                        return True

       Nothing    -> return False


findNextHop :: (HasNetworkStack ns, Network addr)
            => ns
            -> Maybe Device -- ^ Desired output device
            -> Maybe addr   -- ^ Desired source address
            -> addr         -- ^ Destination
            -> IO (Maybe (RouteInfo addr))
findNextHop ns mbDev mbSrc dst =

  -- XXX it might be nice to have lookupRoute return a list of possible routes.
  -- is it unreasonable to think that there may be multiple valid routes for a
  -- datagram?
  do mbRoute <- lookupRoute ns dst
     case mbRoute of
       Just ri | maybe True (== riDev    ri) mbDev
              && maybe True (== riSource ri) mbSrc -> return (Just ri)

       _ -> return Nothing


-- Generic ---------------------------------------------------------------------

instance Network Addr where
  pseudoHeader (Addr4 src) (Addr4 dst) = \ prot len -> pseudoHeader src dst prot len
  {-# INLINE pseudoHeader #-}

  lookupRoute ns (Addr4 dst) =
    do ri <- lookupRoute ns dst
       return (fmap (fmap Addr4) ri)
  {-# INLINE lookupRoute #-}

  sendDatagram' ns dev (Addr4 src) (Addr4 dst) (Addr4 next) =
    sendDatagram' ns dev src dst next
  {-# INLINE sendDatagram' #-}



-- IP4 -------------------------------------------------------------------------

instance Network IP4.IP4 where
  pseudoHeader = IP4.ip4PseudoHeader
  {-# INLINE pseudoHeader #-}

  lookupRoute ns ip4 =
    do mb <- IP4.lookupRoute4 (view networkStack ns) ip4
       case mb of
         Just (riSource,riNext,riDev) -> return $! Just RouteInfo { .. }
         Nothing                      -> return Nothing
  {-# INLINE lookupRoute #-}

  sendDatagram' ns dev src dst next =
    IP4.primSendIP4 (view networkStack ns) dev src dst next
  {-# INLINE sendDatagram' #-}
