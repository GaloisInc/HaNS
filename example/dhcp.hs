{-# LANGUAGE CPP #-}

module Main where

import Hans.Setup

import Hans.Address
import Hans.Address.IP4
import Hans.Address.Mac
import Hans.Layer.Ethernet
import Hans.Layer.Icmp4
import Hans.Layer.Udp
import Hans.Message.Udp
import Hans.Message.Dhcp4
import Hans.Message.Ip4
import Hans.Message.Dhcp4Options
import Hans.Message.Dhcp4Codec
import Hans.Message.EthernetFrame
import Hans.Utils (Packet)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Serialize.Get
import Data.Serialize.Put

import qualified Data.ByteString as S

#ifdef xen_HOST_OS
import Hans.Device.Xen
import Hypervisor.Debug
import Hypervisor.Kernel
import XenDevice.NIC
#else
import Hans.Device.Tap
#endif

output   :: String -> IO ()
outputBS :: S.ByteString -> IO ()

#ifdef xen_HOST_OS
output str = writeDebugConsole (showString str "\n")
outputBS  = output . map (toEnum . fromEnum) . S.unpack
#else
output = putStrLn
outputBS = S.putStrLn
#endif

ip :: IP4
ip  = IP4 192 168 80 9

broad :: IP4
broad  = IP4 255 255 255 255

dst :: IP4
dst  = IP4 192 168 80 10

mac :: Mac
mac = Mac 0x52 0x54 0x00 0x12 0x34 0x56

bootpc = UdpPort 68
bootps = UdpPort 67

udpProtocol = IP4Protocol 0x11

ethernet = EtherType 0x800

zeroAddr = IP4 0 0 0 0

broadcastMac  = Mac 0xff 0xff 0xff 0xff 0xff 0xff

initEthernetDevice :: NetworkStack -> IO ()
#ifdef xen_HOST_OS
initEthernetDevice ns = do
  Just nic <- openXenDevice ""
  addEthernetDevice (nsEthernet ns) mac (xenSend nic) (xenReceiveLoop nic)
#else
initEthernetDevice ns = do
  Just dev <- openTapDevice "tap0"
  addEthernetDevice (nsEthernet ns) mac (tapSend dev) (tapReceiveLoop dev)
#endif


main :: IO ()
#ifdef xen_HOST_OS
main = halvm_kernel [dNICs] $ \ _ -> do
  writeDebugConsole "hans2 test started\n"
#else
main = do
#endif
  ns  <- setup
  initEthernetDevice ns
  apply [ toOption (LocalEthernet (IP4Mask broad 0) mac)   -- ip/24 network
        , toOption (Route (zeroAddr `withMask` 0) ip) -- default route
        ] ns
  startEthernetDevice (nsEthernet ns) mac
  addUdpHandler (nsUdp ns) bootps (simpleDhcpServerHandler ns)
  forever $ threadDelay (1000 * 1000)

simpleDhcpServerHandler ns remoteaddr remoteport packet
  | remoteport /= bootpc = return ()
  | otherwise = case runGet getDhcp4Message packet of
      Left err -> output err
      Right msg -> case parseDhcpMessage msg of
          Just (Left (RequestMessage req)) -> do
             output (show req)
             let ack = requestToAck simpleDhcpSettings req
             sendResponse ns (ackToMessage ack)
          Just (Left (DiscoverMessage disc)) -> do
             output (show disc)
             let offer = discoverToOffer simpleDhcpSettings disc
             sendResponse ns (offerToMessage offer)
          msg1 -> output (show msg) >> output (show msg1)

simpleDhcpClient ns = do
  let discover = mkDiscover (Xid 0x12345) mac
  sendRequest ns (discoverToMessage discover)

simpleDhcpClientHandler ns remoteaddr remoteport packet
  | remoteport /= bootps = return ()
  | otherwise = case runGet getDhcp4Message packet of
      Left err -> output err
      Right msg -> case parseDhcpMessage msg of
          Just (Right (OfferMessage offer)) -> do
             output (show offer)
             let req = offerToRequest offer
             sendRequest ns (requestToMessage req)
          Just (Right (AckMessage ack)) -> do
             output (show ack)
             output "Install assigned IP address"
          msg1 -> do
             output (show msg)
             output (show msg1)

sendRequest ns resp = do
  ipBytes <- mkIpBytes zeroAddr broad bootpc bootps
               (runPut (putDhcp4Message resp))

  sendEthernet (nsEthernet ns) EthernetFrame
    { etherDest         = dhcp4ClientHardwareAddr resp
    , etherSource       = mac
    , etherType         = ethernet
    , etherData         = ipBytes
    }

sendResponse ns resp = do
  ipBytes <- mkIpBytes ip broad bootps bootpc $ runPut (putDhcp4Message resp)

  sendEthernet (nsEthernet ns) EthernetFrame
    { etherDest         = broadcastMac
    , etherSource       = mac
    , etherType         = ethernet
    , etherData         = ipBytes
    }

mkIpBytes srcAddr dstAddr srcPort dstPort payload = do
  udpBytes <- let udpHeader = UdpHeader srcPort dstPort 0
                  udp       = UdpPacket udpHeader payload
                  mk        = mkIP4PseudoHeader srcAddr dstAddr udpProtocol
              in renderUdpPacket udp mk

  ipBytes  <- let iphdr     = emptyIP4Header udpProtocol srcAddr dstAddr
                  ip        = IP4Packet iphdr udpBytes
              in renderIP4Packet ip

  return ipBytes

simpleDhcpSettings :: ServerSettings
simpleDhcpSettings = Settings
  { staticClientAddr = IP4 192 168 80 10
  , staticServerAddr = ip
  , staticLeaseTime = 3600
  , staticSubnet = SubnetMask 24
  , staticBroadcast = IP4 192 168 80 255
  , staticDomainName = "galois.com"
  , staticRouters = [IP4 192 168 80 9]
  , staticDNS = [IP4 192 168 80 11]
  , staticTimeOffset = 12345
  }
