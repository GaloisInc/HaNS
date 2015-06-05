{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hans.Address
import Hans.DhcpClient
import Hans.Address.Mac
import Hans.Address.IP4
import Hans.NetworkStack

#ifdef HaLVM_HOST_OS
import Hans.Device.Xen
import Hypervisor.Console
import Hypervisor.XenStore
import XenDevice.NIC
#else
import Hans.Device.Tap
#endif

import Control.Concurrent (threadDelay,forkIO)

import Control.Monad (forever)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
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
  if args == ["dhcp"]
     then do putStrLn "Discovering address"
             mbIP <- dhcpDiscover ns 10 mac
             case mbIP of
               Nothing -> putStrLn "Couldn't get an IP address."
               Just ip -> do
                 putStrLn ("Bound to address: " ++ show ip)

                 -- putStrLn "Looking up galois.com..."
                 -- HostEntry { .. } <- getHostByName ns "galois.com"
                 -- print hostAddresses


     else do setAddress mac ns

  server ns

server :: NetworkStack -> IO ()
server ns = do
  sock <- listen ns localAddr 9001

  forever $ do
    putStrLn "Waiting..."
    conn <- accept sock
    _    <- forkIO (handleClient conn)
    return ()

handleClient :: Socket -> IO ()
handleClient conn =
  do putStrLn ("Got one: " ++ show (sockRemoteHost conn))
     loop `X.catch` \se -> do print (se :: X.SomeException)
                              close conn
  where
  loop =
    do buf <- recvBytes conn 512
       if L.null buf
          then    putStrLn "Client closed connection"
          else do _ <- sendBytes conn buf
                  loop

message :: L.ByteString
message  = "Hello, world\n"

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
