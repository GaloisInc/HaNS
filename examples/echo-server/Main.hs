{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hans
import Hans.Dns
import Hans.Device
import Hans.IP4.Packet (unpackIP4)
import Hans.IP4.Dhcp.Client (DhcpLease(..),defaultDhcpConfig,dhcpClient)

import           Control.Concurrent (forkIO,threadDelay)
import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as S8
import           System.Environment (getArgs)


main :: IO ()
main  =
  do args        <- getArgs
     (name,dhcp) <- case args of
                      [name,"dhcp"] -> return (S8.pack name,True)
                      name:_        -> return (S8.pack name,False)
                      _             -> fail "Expected a device name"

     ns  <- newNetworkStack defaultConfig
     dev <- addDevice ns name defaultDeviceConfig

     _ <- forkIO $ forever $ do threadDelay 1000000
                                dumpStats (devStats dev)

     _ <- forkIO (processPackets ns)

     -- start receiving data
     startDevice dev

     if dhcp
        then
          do mbLease <- dhcpClient ns defaultDhcpConfig dev
             case mbLease of

               Just lease ->
                 do putStrLn ("Assigned IP: " ++ show (unpackIP4 (dhcpAddr lease)))
                    print =<< getHostByName ns "galois.com"
                    threadDelay (1000000 * 60)

               Nothing ->
                    putStrLn "Dhcp failed"

        else
          do putStrLn "Static IP: 192.168.71.11/24"

             addIP4Route ns False Route
               { routeNetwork = IP4Mask (packIP4 192 168 71 11) 24
               , routeType    = Direct
               , routeDevice  = dev
               }

             addIP4Route ns True Route
               { routeNetwork = IP4Mask (packIP4 192 168 71 11) 0
               , routeType    = Indirect (packIP4 192 168 71 1)
               , routeDevice  = dev
               }

             threadDelay (1000000 * 60)
