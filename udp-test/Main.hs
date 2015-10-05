{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hans.Address
import           Hans.Channel
import           Hans.DhcpClient
import           Hans.Address.Mac
import           Hans.Address.IP4
import           Hans.NetworkStack hiding (NetworkStack (..), newNetworkStack)
import qualified Hans.Layer.Arp as Arp
import qualified Hans.Layer.Ethernet as Eth
import qualified Hans.Layer.Dns as Dns
import qualified Hans.Layer.Icmp4 as Icmp4
import qualified Hans.Layer.IP4 as IP4
import qualified Hans.Layer.Udp as Udp
import           Hans.Message.Udp

#ifdef HaLVM_HOST_OS
import Hans.Device.Xen
import Hypervisor.Console
import Hypervisor.XenStore
import XenDevice.NIC
#else
import Hans.Device.Tap
#endif

import Data.IORef
import Control.Concurrent (threadDelay,forkIO)
import Control.Monad (forever)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S
import qualified Control.Exception as X

localAddr :: IP4
localAddr  = IP4 192 168 90 2

main :: IO ()
main  = do

  ns  <- newNetworkStack
  mac <- initEthernetDevice ns
  deviceUp ns mac
  putStrLn "Network stack running..."

  args <- getArgs
  if args /= ["dhcp"]
     then do setAddress mac ns
     else do putStrLn "Discovering address"
             mbIP <- dhcpDiscover ns mac
             case mbIP of
               Nothing -> do
                 putStrLn "Couldn't get an IP address."
                 setAddress mac ns
               Just ip -> do
                 putStrLn ("Bound to address: " ++ show ip)

  server ns

echoPort :: UdpPort
echoPort = UdpPort 9001

echoHandler :: IORef Int -> NetworkStack -> Udp.Handler
echoHandler counter ns ip port msg = do
  modifyIORef counter (+1)
  sendUdp ns ip (Just echoPort) port (L.fromStrict msg)

server :: NetworkStack -> IO ()
server ns = do
  counter <- newIORef 0

  addUdpHandler ns echoPort (echoHandler counter ns)

  forever $ do
    sleep 5
    count <- readIORef counter
    putStrLn $ unwords ["Received", show count, "messages."]

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000 * 1000)

setAddress :: Mac -> NetworkStack -> IO ()
setAddress mac ns = do
  addIP4Addr ns (localAddr `withMask` 24) mac 1500
  routeVia ns (IP4 0 0 0 0 `withMask` 0) (IP4 192 168 90 1)

#ifdef HaLVM_HOST_OS
initEthernetDevice :: NetworkStack -> IO Mac
initEthernetDevice ns =
  do xs <- initXenStore
     _ <- initXenConsole -- should set up putStrLn
     nics <- listNICs xs
     case nics of
       [] -> fail "No NICs found to use!"
       (macstr:_) ->
         do let mac = read macstr
            nic <- openNIC xs macstr
            addDevice ns mac (xenSend nic) (xenReceiveLoop nic)
            return mac
#else
initEthernetDevice :: NetworkStack -> IO Mac
initEthernetDevice ns = do
  let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x56
  Just dev <- openTapDevice "tap6"
  addDevice ns mac (tapSend dev) (tapReceiveLoop dev)
  return mac
#endif


-----------------------------------------------------------------------
-- Here we reuse some of Hans.NetworkStack, but remove the TCP layer.
-----------------------------------------------------------------------
data NetworkStack = NetworkStack
  { nsArp       :: Arp.ArpHandle
  , nsEthernet  :: Eth.EthernetHandle
  , nsIp4       :: IP4.IP4Handle
  , nsIcmp4     :: Icmp4.Icmp4Handle
  , nsUdp       :: Udp.UdpHandle
  , nsDns       :: Dns.DnsHandle
  }

instance HasArp      NetworkStack where arpHandle      = nsArp
instance HasEthernet NetworkStack where ethernetHandle = nsEthernet
instance HasIP4      NetworkStack where ip4Handle      = nsIp4
instance HasIcmp4    NetworkStack where icmp4Handle    = nsIcmp4
instance HasUdp      NetworkStack where udpHandle      = nsUdp
instance HasDns      NetworkStack where dnsHandle      = nsDns

newNetworkStack :: IO NetworkStack
newNetworkStack  = do
  nsEthernet <- newChannel
  nsArp      <- newChannel
  nsIp4      <- newChannel
  nsIcmp4    <- newChannel
  nsUdp      <- newChannel
  nsDns      <- newChannel

  let ns = NetworkStack { .. }

  startEthernetLayer ns
  startArpLayer      ns
  startIcmp4Layer    ns
  startIP4Layer      ns
  startUdpLayer      ns
  startDnsLayer      ns

  return ns

