{-# LANGUAGE CPP #-}

module Main where

import WebServer

import Hans.Address
import Hans.Address.IP4
import Hans.Address.Mac
import Hans.DhcpClient (dhcpDiscover)
import Hans.Layer.Ethernet
import Hans.Message.Tcp (TcpPort(..))
import Hans.NetworkStack

import System.Exit (exitFailure)
import qualified Data.ByteString as S

#ifdef xen_HOST_OS
import Communication.IVC (InChannelEx,OutChannelEx,Bin)
import Hans.Device.Ivc
import Hans.Device.Xen
import Hypervisor.Debug
import Hypervisor.Kernel
import RendezvousLib.PeerToPeer (p2pConnection)
import XenDevice.NIC
#else
import Hans.Device.Tap
import System.Environment (getArgs)
#endif

output   :: String -> IO ()
outputBS :: S.ByteString -> IO ()

#ifdef xen_HOST_OS
output str = writeDebugConsole (showString str "\n")
outputBS  = output . map (toEnum . fromEnum) . S.unpack

_buildInput :: IO (OutChannelEx Bin Bytes)
buildInput  :: IO (InChannelEx Bin Bytes)
(_buildInput,buildInput) = p2pConnection "ethernet_dev_input"

_buildOutput :: IO (InChannelEx Bin Bytes)
buildOutput  :: IO (OutChannelEx Bin Bytes)
(_buildOutput,buildOutput) = p2pConnection "ethernet_dev_output"

#else
output = putStrLn
outputBS = S.putStrLn
#endif



initEthernetDevice :: NetworkStack -> IO Mac
#ifdef xen_HOST_OS
initEthernetDevice ns = do
  Just nic <- openXenDevice ""
  let mac = read (getNICName nic)
  print mac
  addDevice mac (xenSend nic) (xenReceiveLoop nic) ns
  --let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x56
  --putStrLn "Waiting for input channel..."
  --input  <- buildInput
  --putStrLn "Waiting for output channel..."
  --output <- buildOutput
  --addEthernetDevice (nsEthernet ns) mac (ivcSend output) (ivcReceiveLoop input)
  return mac
#else
initEthernetDevice ns = do
  let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x56
  Just dev <- openTapDevice "tap0"
  addDevice mac (tapSend dev) (tapReceiveLoop dev) ns
  return mac
#endif

main :: IO ()
#ifdef xen_HOST_OS
main = halvm_kernel [dNICs] $ \ args -> do
#else
main = do
  args <- getArgs
#endif
  ns  <- newNetworkStack
  mac <- initEthernetDevice ns
  deviceUp mac ns
  setAddress args mac ns
  webserver ns (TcpPort 8000)

setAddress :: [String] -> Mac -> NetworkStack -> IO ()
setAddress args mac ns =
  case args of
    ["dhcp"] -> dhcpDiscover ns mac print
    [ip,gw]  -> do
      addIP4Addr (read ip `withMask` 24) mac ns
      routeVia (IP4 0 0 0 0 `withMask` 0) (read gw)
    _        -> do
      putStrLn "Usage: <prog> dhcp"
      putStrLn "       <prog> <ip> <gateway>"
      exitFailure
