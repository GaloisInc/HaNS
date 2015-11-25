module Main where

import Hans
import Hans.Device
import Hans.IP4.Dhcp.Client (DhcpLease(..),defaultDhcpConfig,dhcpClient)

import           Control.Concurrent (forkIO,threadDelay)
import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as S8
import           System.Environment (getArgs)


main :: IO ()
main  =
  do args <- getArgs
     name <- case args of
               [name] -> return (S8.pack name)
               _      -> fail "Expected a device name"

     ns  <- newNetworkStack defaultConfig
     dev <- addDevice ns name defaultDeviceConfig

     _ <- forkIO $ forever $ do threadDelay 1000000
                                dumpStats (devStats dev)

     _ <- forkIO (processPackets ns)

     -- start receiving data
     startDevice dev

     mbLease <- dhcpClient ns defaultDhcpConfig dev
     case mbLease of
       Just lease -> putStrLn ("Assigned IP: " ++ show (dhcpAddr lease))
       Nothing    -> putStrLn "Dhcp failed"

