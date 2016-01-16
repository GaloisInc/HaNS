{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hans
import Hans.Dns
import Hans.Device
import Hans.Socket
import Hans.IP4.Packet (pattern WildcardIP4)
import Hans.IP4.Dhcp.Client (DhcpLease(..),defaultDhcpConfig,dhcpClient)

import           Control.Concurrent (forkIO,threadDelay)
import           Control.Exception
import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.Environment (getArgs)
import           System.Exit (exitFailure)


main :: IO ()
main  =
  do args        <- getArgs
     (name,dhcp) <- case args of
                      [name,"dhcp"] -> return (S8.pack name,True)
                      name:_        -> return (S8.pack name,False)
                      _             -> fail "Expected a device name"

     ns  <- newNetworkStack defaultConfig
     dev <- addDevice ns name defaultDeviceConfig

     _ <- forkIO (showExceptions "processPackets" (processPackets ns))

     -- start receiving data
     startDevice dev

     if dhcp
        then
          do mbLease <- dhcpClient ns defaultDhcpConfig dev
             case mbLease of

               Just lease ->
                 do putStrLn ("Assigned IP: " ++ show (unpackIP4 (dhcpAddr lease)))
                    print =<< getHostByName ns "galois.com"

               Nothing ->
                 do putStrLn "Dhcp failed"
                    exitFailure

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

     sock <- sListen ns defaultSocketConfig WildcardIP4 9001 10
     putStrLn "Waiting for a client"
     client <- showExceptions "sAccept" (sAccept (sock :: TcpListenSocket IP4))
     putStrLn "Got a client"
     handleClient client

     threadDelay (10 * 1000000)

     dumpStats (devStats dev)

     putStrLn "done?"


showExceptions :: String -> IO a -> IO a
showExceptions l m = m `catch` \ e ->
  do print (l, e :: SomeException)
     throwIO e


handleClient :: TcpSocket IP4 -> IO ()
handleClient sock = loop `finally` sClose sock
  where
  loop =
    do str <- sRead sock 1024
       if L8.null str
          then putStrLn "Closing client"
          else do sWrite sock str
                  sWrite sock "foo\n"
                  loop
