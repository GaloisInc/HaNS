{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Network (
    module Hans.Network,
    module Hans.Network.Types
  ) where

import           Hans.Addr (IsAddr,IP6,IP4,toIP4,toIP6)
import           Hans.Checksum (PartialChecksum)
import           Hans.Device.Types (Device)
import qualified Hans.IP4        as IP4
import qualified Hans.IP4.State  as IP4
import qualified Hans.IP6        as IP6
import           Hans.Lens
import           Hans.Network.Types
import           Hans.Types

import qualified Data.ByteString.Lazy as L


-- | Interaction with routing and message delivery for a network layer.
class IsAddr addr => Network addr where
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
                -> Bool -- ^ Don't fragment
                -> NetworkProtocol
                -> L.ByteString
                -> IO ()


sendDatagram :: (HasNetworkStack ns, Network addr)
             => ns -> RouteInfo addr -> addr
             -> Bool -> NetworkProtocol -> L.ByteString
             -> IO ()
sendDatagram ns RouteInfo { .. } = \ dst ->
  sendDatagram' ns riDev riSource dst riNext
{-# INLINE sendDatagram #-}



-- | Send a datagram and lookup routing information at the same time. Returns
-- 'False' if no route to the destination was known.
routeDatagram :: (HasNetworkStack ns, Network addr)
              => ns -> addr -> Bool -> NetworkProtocol -> L.ByteString -> IO Bool
routeDatagram ns dst df prot bytes =
  do mbRoute <- lookupRoute ns dst
     case mbRoute of
       Just route -> do sendDatagram ns route dst df prot bytes
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

-- XXX: (ACW) I'm not sure this guessing about whether or not things should be
-- interpreted is a good idea. Actually, it seems like a bad idea.
instance Network IP6 where
  pseudoHeader src dst =
    case (toIP4 src, toIP4 dst) of
      (Just a, Just b)   -> pseudoHeader a b
      (Nothing, Nothing) -> IP6.ip6PseudoHeader src dst
      _                  -> error "pseudoHeader: src and dest are on different networks"

  lookupRoute ns dst =
    case toIP4 dst of
      Just ip4 ->
        do ri <- lookupRoute ns ip4
           return (fmap (fmap toIP6) ri)

      Nothing ->
           error "lookupRoute: IP6 not implemented"
  {-# INLINE lookupRoute #-}

  sendDatagram' ns dev src dst next =
    case (toIP4 src, toIP4 dst, toIP4 next) of
      (Just a, Just b, Just c)    -> sendDatagram' ns dev a b c
      (Nothing, Nothing, Nothing) -> error "sendDatagram': IP6 not implemented"
      _                           -> error "sendDatagram': src dest or next on different networks"
  {-# INLINE sendDatagram' #-}



-- IP4 -------------------------------------------------------------------------

instance Network IP4 where
  pseudoHeader = IP4.ip4PseudoHeader
  {-# INLINE pseudoHeader #-}

  lookupRoute ns ip4 =
    do mb <- IP4.lookupRoute4 (view networkStack ns) ip4
       case mb of
         Just (riSource,riNext,riDev) -> return $! Just RouteInfo { .. }
         Nothing                      -> return Nothing
  {-# INLINE lookupRoute #-}

  sendDatagram' ns dev src dst df next =
    IP4.primSendIP4 (view networkStack ns) dev src dst df next
  {-# INLINE sendDatagram' #-}
