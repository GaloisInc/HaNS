{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.Dhcp.Client (
    DhcpConfig(..),
    defaultDhcpConfig,
    DhcpLease(..),
    dhcpClient,
  ) where

import Hans.Device.Types (Device(devMac))
import Hans.IP4.Dhcp.Packet
import Hans.IP4.Packet (IP4,pattern WildcardIP4,pattern BroadcastIP4)
import Hans.Socket
           (DatagramSocket,sOpen,sClose,sendto4,recvfrom4,SockPort
           ,defaultSocketConfig)
import Hans.Serialize (runPutPacket)
import Hans.Time (toUSeconds)
import Hans.Types (NetworkStack)

import qualified Data.ByteString.Lazy as L
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
  go retries to =
    do send
       mb <- timeout to recv
       case mb of

         Just{} -> return mb

         -- adjust the timeout by two, and add some slack before trying again
         Nothing | retries > 0 ->
           do slack <- randomRIO (500,1000)
              go (retries - 1) (to * 2 + slack * 1000)

         _ -> return Nothing


data DhcpLease = DhcpLease { dhcpRenew :: !(IO ())
                           , dhcpAddr  :: !IP4
                           }


dhcpClient :: NetworkStack -> DhcpConfig -> Device -> IO (Maybe DhcpLease)
dhcpClient ns cfg dev =
  do sock <- sOpen ns defaultSocketConfig (Just dev) WildcardIP4 (Just bootpc)
     dhcpDiscover cfg dev sock


-- | Discover a dhcp server, and request an address.
dhcpDiscover :: DhcpConfig -> Device -> DatagramSocket IP4 -> IO (Maybe DhcpLease)
dhcpDiscover cfg dev sock =
  do xid  <- mkXid
     let msg = renderMessage (discoverToMessage (mkDiscover xid (devMac dev)))

     mb <- waitResponse cfg (sendto4 sock BroadcastIP4 bootps msg) (awaitOffer sock)
     case mb of
       Just offer -> dhcpRequest cfg sock offer
       Nothing    -> do sClose sock
                        return Nothing


-- | Only accept an offer.
awaitOffer :: DatagramSocket IP4 -> IO Offer
awaitOffer sock = go
  where
  go =
    do (_,_,srcPort,bytes) <- recvfrom4 sock

       if srcPort /= bootps
          then go
          else case runGetLazy getDhcp4Message bytes of

                 Right msg
                   | Just (Right (OfferMessage o)) <- parseDhcpMessage msg ->
                     return o

                 _ -> go


-- | Respond to an offer with a request, and configure the network stack if an
-- acknowledgement is received.
dhcpRequest :: DhcpConfig -> DatagramSocket IP4 -> Offer -> IO (Maybe DhcpLease)
dhcpRequest cfg sock offer =
  do let req = renderMessage (requestToMessage (offerToRequest offer))
     mb <- waitResponse cfg (sendto4 sock BroadcastIP4 bootps req) (awaitAck sock)
     sClose sock
     case mb of

       Nothing  -> return Nothing

       -- XXX apply all the routing information
       Just ack -> undefined


awaitAck :: DatagramSocket IP4 -> IO Ack
awaitAck sock = go
  where
  go =
    do (_,_,srcPort,bytes) <- recvfrom4 sock

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
  do sock <- sOpen ns defaultSocketConfig (Just dev) WildcardIP4 (Just bootps)
     _    <- dhcpRequest cfg sock offer

     return ()
