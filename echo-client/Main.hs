{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Concurrent (threadDelay)
import Hans.Address.Mac
import Hans.DhcpClient
import Hans.NetworkStack
import System.Environment (getArgs)

#ifdef HaLVM_HOST_OS
import Hans.Device.Xen
import Hypervisor.Console
import Hypervisor.XenStore
import XenDevice.NIC
#else
import Hans.Device.Tap
#endif

main :: IO ()
main  = do

  [ip] <- getArgs
  let addr = read ip

  ns  <- newNetworkStack
  mac <- initEthernetDevice ns

  deviceUp ns mac
  putStrLn "Network stack running..."

  putStrLn "Discovering address"

  mbIP <- dhcpDiscover ns mac
  case mbIP of
    Nothing -> putStrLn "Couldn't get an IP address."
    Just self -> do
      putStrLn ("Bound to address: " ++ show self)

      sock <- connect ns addr 9001 Nothing
      putStrLn ("Connected to: " ++ show addr)

      putStrLn "Sending bytes..."
      sent <- sendBytes sock "Hello"

      putStrLn ("Sent " ++ show sent ++ " bytes")
      print =<< recvBytes sock 512

      threadDelay 1000000

      close sock

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
  let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x57
  Just dev <- openTapDevice "tap7"
  addDevice ns mac (tapSend dev) (tapReceiveLoop dev)
  return mac
#endif
