{-# LANGUAGE CPP #-}

module Main where

import WebServer

import Hans.Address
import Hans.Address.IP4
import Hans.Address.Mac
import Hans.DhcpClient (dhcpDiscover)
import Hans.Layer.Ethernet
import Hans.Message.Tcp (TcpPort(..))
import Hans.Setup

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
  addEthernetDevice (nsEthernet ns) mac (xenSend nic) (xenReceiveLoop nic)
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
  addEthernetDevice (nsEthernet ns) mac (tapSend dev) (tapReceiveLoop dev)
  return mac
#endif

main :: IO ()
#ifdef xen_HOST_OS
main = halvm_kernel [dNICs] $ \ args -> do
#else
main = do
  args <- getArgs
#endif
  ns  <- setup
  mac <- initEthernetDevice ns
  startEthernetDevice (nsEthernet ns) mac
  setAddress args mac ns
  webserver ns (TcpPort 8000)

setAddress :: [String] -> Mac -> NetworkStack -> IO ()
setAddress args mac ns =
  case args of
    ["dhcp"] -> dhcpDiscover ns mac print
    [ip,gw]  -> apply (addrOptions ip gw mac) ns
    _        -> do
      putStrLn "Usage: <prog> dhcp"
      putStrLn "       <prog> <ip> <gateway>"
      exitFailure

addrOptions :: String -> String -> Mac -> [SomeOption]
addrOptions ip gw mac =
  [ SomeOption (LocalEthernet (ip4 `withMask` 24) mac)
  , SomeOption (Route (IP4 0 0 0 0 `withMask` 0) gw4)
  ]
  where
  ip4 :: IP4
  ip4  = read ip
  gw4 :: IP4
  gw4  = read gw
