module Hans.DhcpClient (
    dhcpDiscover
  ) where

import Hans.Address
import Hans.Address.IP4 (IP4(..),broadcastIP4,IP4Mask(..))
import Hans.Address.Mac (Mac(..),broadcastMac)
import Hans.Layer.Ethernet (sendEthernet,addEthernetHandler)
import Hans.Layer.IP4 (connectEthernet)
import Hans.Message.Dhcp4
import Hans.Message.Dhcp4Codec
import Hans.Message.Dhcp4Options
import Hans.Message.EthernetFrame
import Hans.Message.Ip4
import Hans.Message.Udp
import Hans.NetworkStack
import Hans.Timers (delay_)

import Control.Monad (guard,unless)
import Control.Concurrent.STM
import Data.Maybe (fromMaybe,mapMaybe)
import System.Random (randomIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Protocol Constants ----------------------------------------------------------

-- | BOOTP server port.
bootps :: UdpPort
bootps  = UdpPort 67

-- | BOOTP client port.
bootpc :: UdpPort
bootpc  = UdpPort 68

currentNetwork :: IP4
currentNetwork  = IP4 0 0 0 0

ethernetIp4 :: EtherType
ethernetIp4  = EtherType 0x0800

defaultRoute :: IP4Mask
defaultRoute  = IP4 0 0 0 0 `withMask` 0


-- DHCP ------------------------------------------------------------------------

-- | Discover a dhcp server, and request an address.
dhcpDiscover :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
                , HasDns stack )
             => stack -> Int -> Mac -> IO (Maybe IP4)
dhcpDiscover ns retries mac = do
  addEthernetHandler (ethernetHandle ns) ethernetIp4 (dhcpIP4Handler ns)

  offerTMV <- newEmptyTMVarIO
  addUdpHandler ns bootpc (handleOffer ns offerTMV)

  w32 <- randomIO
  let xid = Xid (fromIntegral (w32 :: Int))
      disc = discoverToMessage (mkDiscover xid mac)

  --  waitResult sends our DHCP discover and waits for an offer.
  mbOffer <- waitResult retries disc offerTMV
  case mbOffer of

    -- We exceeded our retries, give up.
    Nothing    -> return Nothing

    --  We got an offer
    --  - Install an DHCP Ack handler.
    --  - Send a DHCP Request via waitResult, and wait for an IP to appear in resp.
    Just offer -> do
      resp <- newEmptyTMVarIO
      addUdpHandler ns bootpc (handleAck ns offer (Just (atomically . putTMVar resp)))
      let req = requestToMessage (offerToRequest offer)
      waitResult retries req resp

  where
    initialTimeout :: Int
    initialTimeout = 10000000 -- 10 seconds in µs

    -- Sends a message and waits for a response (indicated by a value appearing the TMVar)
    -- until timeout. Retries a given number of times doubling the backoff each time.
    waitResult :: Int -> Dhcp4Message -> TMVar a -> IO (Maybe a)
    waitResult n msg result = go n initialTimeout
      where
        go 0 _to = return Nothing
        go i  to =
          do sendMessage ns msg currentNetwork broadcastIP4 broadcastMac
             timeout <- registerDelay to
             -- If there is a result, return Just it.
             -- If not, and we aren't timed out, retry.
             -- If we are timed out, return Nothing.
             mb <- atomically $ orElse (fmap Just (takeTMVar result))
                                       (do isTimedOut <- readTVar timeout
                                           unless isTimedOut retry
                                           return Nothing)
             -- Exponential backoff on timeout, return result on success.
             case mb of
               Just _  -> return mb
               Nothing -> go (i-1) (to * 2)

-- | Restore the connection between the Ethernet and IP4 layers.
restoreIp4 :: (HasEthernet stack, HasIP4 stack) => stack -> IO ()
restoreIp4 ns = connectEthernet (ip4Handle ns) (ethernetHandle ns)

-- | Handle IP4 messages from the Ethernet layer, passing all relevant DHCP
-- messages to the UDP layer.
dhcpIP4Handler :: (HasUdp stack)
               => stack -> S.ByteString -> IO ()
dhcpIP4Handler ns bytes =
  case parseIP4Packet bytes of
    Left err            -> putStrLn err >> return ()
    Right (hdr,ihl,len)
      | ip4Protocol hdr == udpProtocol -> queue
      | otherwise                      -> return ()
      where
      queue = queueUdp ns hdr
            $ S.take (len - ihl)
            $ S.drop ihl bytes

-- | Handle a DHCP Offer message.
--
--  * Remove the current UDP handler
--  * Write offer to TMV for retrieval.
handleOffer :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
               , HasDns stack )
            => stack -> TMVar Offer -> IP4 -> UdpPort
            -> S.ByteString -> IO ()
handleOffer ns tmv _src _srcPort bytes =
  case getDhcp4Message bytes of
    Right msg -> case parseDhcpMessage msg of

      Just (Right (OfferMessage offer)) -> do
        removeUdpHandler ns bootpc
        atomically $ putTMVar tmv offer

      msg1 -> do
        putStrLn (show msg)
        putStrLn (show msg1)

    Left err -> putStrLn err

-- | Handle a DHCP Ack message.
--
--   The optional handler (mbh) is present only the first time handleAck is
--   installed, so we can update the main thread on our IP assignment. After
--   that (when dhcpRenew installs handleAck) it is Nothing, because we just
--   have to update the network stack.
--
--  * Remove the custom IP4 handler
--  * Restore the connection between the Ethernet and IP4 layers
--  * Remove the bootpc UDP listener
--  * Configure the network stack with options from the Ack
--  * Install a timer that renews the address after 50% of the lease time
--    has passed
handleAck :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
             , HasDns stack )
          => stack -> Offer -> Maybe (IP4 -> IO ()) -> IP4 -> UdpPort
          -> S.ByteString -> IO ()
handleAck ns offer mbh _src _srcPort bytes =
  case getDhcp4Message bytes of
    Right msg -> case parseDhcpMessage msg of

      Just (Right (AckMessage ack)) -> do
        removeUdpHandler ns bootpc
        restoreIp4 ns
        ackNsOptions ack ns
        let ms = fromIntegral (ackLeaseTime ack) * 500
        delay_ ms (dhcpRenew ns offer)

        case mbh of
          Nothing -> return ()
          Just h  -> h (ackYourAddr ack)

      msg1 -> do
        putStrLn (show msg)
        putStrLn (show msg1)

    Left err -> putStrLn err

-- | Perform a DHCP Renew.
--
--  * Re-install the DHCP IP4 handler
--  * Add a UDP handler for an Ack message
--  * Re-send a request message, generated from the offer given.
dhcpRenew :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
             , HasDns stack )
          => stack -> Offer -> IO ()
dhcpRenew ns offer = do
  addEthernetHandler (ethernetHandle ns) ethernetIp4 (dhcpIP4Handler ns)

  let req = requestToMessage (offerToRequest offer)
  addUdpHandler ns bootpc (handleAck ns offer Nothing)
  sendMessage ns req currentNetwork broadcastIP4 broadcastMac


-- NetworkStack Config ---------------------------------------------------------

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

-- | Produce options for the network stack from a DHCP Ack.
ackNsOptions :: (HasIP4 stack, HasArp stack, HasDns stack)
             => Ack -> stack -> IO ()
ackNsOptions ack ns = do
  let mac     = ackClientHardwareAddr ack
      addr    = ackYourAddr ack
      opts    = ackOptions ack
      mask    = fromMaybe 24 (lookupSubnet opts)
      gateway = fromMaybe (ackRelayAddr ack) (lookupGateway opts)
  addIP4Addr ns (addr `withMask` mask) mac 1500
  routeVia ns defaultRoute gateway

  let nameServers = concat (mapMaybe getNameServers (ackOptions ack))
  mapM_ (addNameServer ns) nameServers

getNameServers :: Dhcp4Option -> Maybe [IP4]
getNameServers (OptNameServers addrs) = Just addrs
getNameServers _                      = Nothing


-- Packet Helpers --------------------------------------------------------------

sendMessage :: HasEthernet stack
            => stack -> Dhcp4Message -> IP4 -> IP4 -> Mac -> IO ()
sendMessage ns resp src dst hwdst = do
  ipBytes <- mkIpBytes src dst bootpc bootps (putDhcp4Message resp)
  let mac   = dhcp4ClientHardwareAddr resp
  let frame = EthernetFrame
        { etherDest         = hwdst
        , etherSource       = mac
        , etherType         = ethernetIp4
        }
  sendEthernet (ethernetHandle ns) frame ipBytes

mkIpBytes :: IP4 -> IP4 -> UdpPort -> UdpPort -> L.ByteString -> IO L.ByteString
mkIpBytes srcAddr dstAddr srcPort dstPort payload = do
  udpBytes <- do
    let udpHdr = UdpHeader srcPort dstPort 0
        mk     = mkIP4PseudoHeader srcAddr dstAddr udpProtocol
    renderUdpPacket udpHdr payload mk

  ipBytes  <- do
    let ipHdr = emptyIP4Header
          { ip4SourceAddr = srcAddr
          , ip4DestAddr   = dstAddr
          , ip4Protocol   = udpProtocol
          }
    renderIP4Packet ipHdr udpBytes

  return ipBytes
