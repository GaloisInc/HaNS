module Hans.DhcpClient (
    dhcpDiscover
  ) where

import Hans.Address
import Hans.Address.IP4 (IP4(..),broadcastIP4,IP4Mask(..))
import Hans.Address.Mac (Mac(..),broadcastMac)
import Hans.Layer.Ethernet (sendEthernet,addEthernetHandler)
import Hans.Layer.IP4 (connectEthernet)
import Hans.Layer.Timer (delay)
import Hans.Layer.Udp (addUdpHandler,removeUdpHandler,queueUdp)
import Hans.Message.Dhcp4
import Hans.Message.Dhcp4Codec
import Hans.Message.Dhcp4Options
import Hans.Message.EthernetFrame
import Hans.Message.Ip4
import Hans.Message.Udp
import Hans.NetworkStack

import Control.Monad (guard)
import Data.Serialize (runGet,runPutLazy)
import Data.Maybe (fromMaybe)
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

type AckHandler = IP4 -> IO ()

-- | Discover a dhcp server, and request an address.
dhcpDiscover :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
                , HasTimer stack)
             => stack -> Mac -> AckHandler -> IO ()
dhcpDiscover ns mac h = do
  w32 <- randomIO
  let xid = Xid (fromIntegral (w32 :: Int))

  addEthernetHandler (ethernetHandle ns) ethernetIp4 (dhcpIP4Handler ns)
  addUdpHandler (udpHandle ns) bootpc (handleOffer ns (Just h))

  let disc = discoverToMessage (mkDiscover xid mac)
  sendMessage ns disc currentNetwork broadcastIP4 broadcastMac

-- | Restore the connection between the Ethernet and IP4 layers.
restoreIp4 :: (HasEthernet stack, HasIP4 stack) => stack -> IO ()
restoreIp4 ns = connectEthernet (ip4Handle ns) (ethernetHandle ns)

-- | Handle IP4 messages from the Ethernet layer, passing all relevant DHCP
-- messages to the UDP layer.
dhcpIP4Handler :: (HasUdp stack)
               => stack -> S.ByteString -> IO ()
dhcpIP4Handler ns bytes =
  case runGet parseIP4Packet bytes of
    Left err            -> putStrLn err >> return ()
    Right (hdr,ihl,len)
      | ip4Protocol hdr == udpProtocol -> queue
      | otherwise                      -> return ()
      where
      queue = queueUdp (udpHandle ns) (ip4SourceAddr hdr) (ip4DestAddr hdr)
            $ S.take (len - ihl)
            $ S.drop ihl bytes

-- | Handle a DHCP Offer message.
--
--  * Remove the current UDP handler
--  * Install an DHCP Ack handler
--  * Send a DHCP Request
handleOffer :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
               , HasTimer stack)
            => stack -> Maybe AckHandler -> IP4 -> UdpPort
            -> S.ByteString -> IO ()
handleOffer ns mbh _src _srcPort bytes =
  case runGet (getDhcp4Message) bytes of
    Right msg -> case parseDhcpMessage msg of

      Just (Right (OfferMessage offer)) -> do
        removeUdpHandler (udpHandle ns) bootpc
        let req = requestToMessage (offerToRequest offer)
        addUdpHandler (udpHandle ns) bootpc (handleAck ns offer mbh)
        sendMessage ns req currentNetwork broadcastIP4 broadcastMac

      msg1 -> do
        putStrLn (show msg)
        putStrLn (show msg1)

    Left err -> putStrLn err

-- | Handle a DHCP Ack message.
--
--  * Remove the custom IP4 handler
--  * Restore the connection between the Ethernet and IP4 layers
--  * Remove the bootpc UDP listener
--  * Configure the network stack with options from the Ack
--  * Install a timer that renews the address after 50% of the lease time
--    has passed
handleAck :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
             , HasTimer stack)
          => stack -> Offer -> Maybe AckHandler -> IP4 -> UdpPort
          -> S.ByteString -> IO ()
handleAck ns offer mbh _src _srcPort bytes =
  case runGet (getDhcp4Message) bytes of
    Right msg -> case parseDhcpMessage msg of

      Just (Right (AckMessage ack)) -> do
        removeUdpHandler (udpHandle ns) bootpc
        restoreIp4 ns
        ackNsOptions ack ns
        let ms = fromIntegral (ackLeaseTime ack) * 500
        delay (timerHandle ns) ms (dhcpRenew ns offer)
        putStrLn ("Bound to: " ++ show (ackYourAddr ack))

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
--  * Re-send a renquest message, generated from the offer given.
dhcpRenew :: ( HasEthernet stack, HasArp stack, HasIP4 stack, HasUdp stack
             , HasTimer stack)
          => stack -> Offer -> IO ()
dhcpRenew ns offer = do
  addEthernetHandler (ethernetHandle ns) ethernetIp4 (dhcpIP4Handler ns)

  let req = requestToMessage (offerToRequest offer)
  addUdpHandler (udpHandle ns) bootpc (handleAck ns offer Nothing)
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
ackNsOptions :: (HasIP4 stack, HasArp stack) => Ack -> stack -> IO ()
ackNsOptions ack ns = do
  let mac     = ackClientHardwareAddr ack
      addr    = ackYourAddr ack
      opts    = ackOptions ack
      mask    = fromMaybe 24 (lookupSubnet opts)
      gateway = fromMaybe (ackRelayAddr ack) (lookupGateway opts)
  addIP4Addr ns (addr `withMask` mask) mac 1500
  routeVia ns defaultRoute gateway


-- Packet Helpers --------------------------------------------------------------

sendMessage :: (HasEthernet stack)
            => stack -> Dhcp4Message -> IP4 -> IP4 -> Mac -> IO ()
sendMessage ns resp src dst hwdst = do
  ipBytes <- mkIpBytes src dst bootpc bootps
      (runPutLazy (putDhcp4Message resp))
  let mac   = dhcp4ClientHardwareAddr resp
  let frame = EthernetFrame
        { etherDest         = hwdst
        , etherSource       = mac
        , etherType         = ethernetIp4
        }
  putStrLn (show mac ++ " -> " ++ show hwdst)

  sendEthernet (ethernetHandle ns) frame ipBytes

mkIpBytes :: IP4 -> IP4 -> UdpPort -> UdpPort -> L.ByteString -> IO L.ByteString
mkIpBytes srcAddr dstAddr srcPort dstPort payload = do
  udpBytes <- do
    let udpHdr = UdpHeader srcPort dstPort 0
        mk     = mkIP4PseudoHeader srcAddr dstAddr udpProtocol
    renderUdpPacket udpHdr payload mk

  ipBytes  <- do
    let ipHdr = emptyIP4Header udpProtocol srcAddr dstAddr
    renderIP4Packet ipHdr udpBytes

  return ipBytes
