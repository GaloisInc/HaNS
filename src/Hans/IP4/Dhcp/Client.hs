{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.Dhcp.Client (
    DhcpConfig(..),
    defaultDhcpConfig,
    DhcpLease(..),
    dhcpClient,
  ) where

import Hans.Device.Types (Device(devMac))
import Hans.IP4.Dhcp.Codec (SubnetMask(..))
import Hans.IP4.Dhcp.Packet
import Hans.IP4.Dhcp.Options
import Hans.IP4.Packet (IP4,pattern WildcardIP4,pattern BroadcastIP4,IP4Mask(..))
import Hans.IP4.RoutingTable(Route(..),RouteType(..))
import Hans.Lens
import Hans.Socket
           (UdpSocket,newUdpSocket,sClose,sendto,recvfrom,SockPort
           ,defaultSocketConfig)
import Hans.Serialize (runPutPacket)
import Hans.Time (toUSeconds)
import Hans.Types (NetworkStack,networkStack,addRoute,addNameServer4)

import           Control.Concurrent (forkIO,threadDelay,killThread)
import           Control.Monad (when,guard)
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (fromMaybe,mapMaybe)
import           Data.Serialize.Get (runGetLazy)
import           Data.Time.Clock (NominalDiffTime)
import           System.Random (randomIO,randomRIO)
import           System.Timeout (timeout)


-- | BOOTP server port.
bootps :: SockPort
bootps  = 67

-- | BOOTP client port.
bootpc :: SockPort
bootpc  = 68

mkXid :: IO Xid
mkXid  = do w <- randomIO
            return (Xid w)


renderMessage :: Dhcp4Message -> L.ByteString
renderMessage msg = runPutPacket 236 256 L.empty (putDhcp4Message msg)

data DhcpConfig = DhcpConfig { dcInitialTimeout :: !NominalDiffTime
                               -- ^ Initial timeout

                             , dcRetries :: !Int
                               -- ^ Number of retries

                             , dcDefaultRoute :: Bool
                               -- ^ Whether or not routing information received
                               -- from the DHCP server should be used as the
                               -- default route for the network stack.

                             , dcAutoRenew :: Bool
                               -- ^ Whether or not to fork a renew thread once
                               -- configuration information has been received.
                             }

defaultDhcpConfig :: DhcpConfig
defaultDhcpConfig  = DhcpConfig { dcInitialTimeout = 4.0
                                , dcRetries        = 6
                                , dcDefaultRoute   = True
                                , dcAutoRenew      = True }


-- | Wait for a result on a socket
waitResponse :: DhcpConfig -> IO () -> IO a -> IO (Maybe a)
waitResponse DhcpConfig { .. } send recv =
  go dcRetries (toUSeconds dcInitialTimeout)
  where
  go retries toVal =
    do send
       mb <- timeout toVal recv
       case mb of

         Just{} -> return mb

         -- adjust the timeout by two, and add some slack before trying again
         Nothing | retries > 0 ->
           do slack <- randomRIO (500,1000)
              go (retries - 1) (toVal * 2 + slack * 1000)

         _ -> return Nothing


data DhcpLease = DhcpLease { dhcpRenew :: !(IO ())
                           , dhcpAddr  :: !IP4
                           }


dhcpClient :: NetworkStack -> DhcpConfig -> Device -> IO (Maybe DhcpLease)
dhcpClient ns cfg dev =
  do sock <- newUdpSocket ns defaultSocketConfig (Just dev) WildcardIP4 (Just bootpc)
     dhcpDiscover cfg dev sock


-- | Discover a dhcp server, and request an address.
dhcpDiscover :: DhcpConfig -> Device -> UdpSocket IP4 -> IO (Maybe DhcpLease)
dhcpDiscover cfg dev sock =
  do xid  <- mkXid
     let msg = renderMessage (discoverToMessage (mkDiscover xid (devMac dev)))

     mb <- waitResponse cfg (sendto sock BroadcastIP4 bootps msg) (awaitOffer sock)
     case mb of
       Just offer -> dhcpRequest cfg dev sock offer
       Nothing    -> do sClose sock
                        return Nothing


-- | Only accept an offer.
awaitOffer :: UdpSocket IP4 -> IO Offer
awaitOffer sock = go
  where
  go =
    do (_,_,srcPort,bytes) <- recvfrom sock

       if srcPort /= bootps
          then go
          else case runGetLazy getDhcp4Message bytes of

                 Right msg
                   | Just (Right (OfferMessage o)) <- parseDhcpMessage msg ->
                     return o

                 _ -> go


-- | Respond to an offer with a request, and configure the network stack if an
-- acknowledgement is received.
dhcpRequest :: DhcpConfig -> Device -> UdpSocket IP4 -> Offer -> IO (Maybe DhcpLease)
dhcpRequest cfg dev sock offer =
  do let req = renderMessage (requestToMessage (offerToRequest offer))
     mb <- waitResponse cfg (sendto sock BroadcastIP4 bootps req) (awaitAck sock)
     sClose sock
     case mb of

       Nothing  -> return Nothing

       Just ack ->
         do lease <- handleAck (view networkStack sock) cfg dev offer ack
            return (Just lease)


awaitAck :: UdpSocket IP4 -> IO Ack
awaitAck sock = go
  where
  go =
    do (_,_,srcPort,bytes) <- recvfrom sock

       if srcPort /= bootps
          then go
          else case runGetLazy getDhcp4Message bytes of

                 Right msg
                   | Just (Right (AckMessage a)) <- parseDhcpMessage msg ->
                     return a

                 _ -> go



-- | Perform a DHCP Renew.
renew :: NetworkStack -> DhcpConfig -> Device -> Offer -> IO ()
renew ns cfg dev offer =
  do sock <- newUdpSocket ns defaultSocketConfig (Just dev) WildcardIP4 (Just bootps)
     _    <- dhcpRequest cfg dev sock offer

     return ()


-- Ack Management --------------------------------------------------------------

-- | Apply the information in the Ack to the NetworkStack, and Device. Returns
-- information about the lease, as well as an IO action that can be used to
-- renew it.
handleAck :: NetworkStack -> DhcpConfig -> Device -> Offer -> Ack -> IO DhcpLease
handleAck ns cfg dev offer Ack { .. } =
  do let addr     = ackYourAddr
         mask     = fromMaybe 24 (lookupSubnet ackOptions)
         (ty,def) = case lookupGateway ackOptions of
                      Just gw -> (Indirect gw,dcDefaultRoute cfg)
                      Nothing -> (Direct,False)

     let nameServers = concat (mapMaybe getNameServers ackOptions)
     mapM_ (addNameServer4 ns) nameServers

     addRoute ns False Route
       { routeNetwork = IP4Mask addr mask
       , routeType    = ty
       , routeDevice  = dev
       }

     when def $
       addRoute ns True Route
         { routeNetwork = IP4Mask addr 0
         , routeType    = ty
         , routeDevice  = dev
         }

     dhcpRenew <-
       if dcAutoRenew cfg
          then -- wait for half of the lease time, then automatically renew
               -- XXX: what happens on a 32-bit system here?
               do tid <- forkIO $
                         do threadDelay (fromIntegral ackLeaseTime * 500000)
                            renew ns cfg dev offer

                  return $ do killThread tid
                              renew ns cfg dev offer

          else return (renew ns cfg dev offer)

     return $! DhcpLease { dhcpAddr = addr, .. }


lookupGateway :: [Dhcp4Option] -> Maybe IP4
lookupGateway  = foldr p Nothing
  where
  p (OptRouters rs) _ = guard (not (null rs)) >> Just (head rs)
  p _               a = a

lookupSubnet :: [Dhcp4Option] -> Maybe Int
lookupSubnet  = foldr p Nothing
  where
  p (OptSubnetMask (SubnetMask i)) _ = Just i
  p _                              a = a

getNameServers :: Dhcp4Option -> Maybe [IP4]
getNameServers (OptNameServers addrs) = Just addrs
getNameServers _                      = Nothing
