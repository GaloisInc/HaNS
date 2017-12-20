{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
module Main where

import           Hans
import           Hans.Device
import           Hans.IP4.Dhcp.Client       (DhcpLease(..),defaultDhcpConfig,dhcpClient)
import           Hans.IP4.Packet            (pattern WildcardIP4)
import           Hans.Socket
import           System.IO

import           Control.Concurrent         (forkIO,threadDelay)
import           Control.Exception
import           Control.Monad              (forever,void)
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)

main :: IO ()
main =
  getArgs >>= \case
    [name] -> run (S8.pack name)
    _ -> fail "Expected a device name (i.e. en0)"

run :: S8.ByteString -> IO ()
run name = do
     ns <- newNetworkStack defaultConfig
     dev <- addDevice ns name defaultDeviceConfig
     _ <- forkIO (showExceptions "processPackets" (processPackets ns))
     startDevice dev
     mbLease <- dhcpClient ns defaultDhcpConfig dev
     case mbLease of
       Just lease ->
         putStrLn ("Assigned IP: " ++ show (unpackIP4 (dhcpAddr lease)))
       Nothing -> do
         putStrLn "Dhcp failed"
         exitFailure

     sock <- sListen ns defaultSocketConfig WildcardIP4 8080 10
     void . forkIO . forever $ do
       putStrLn "Waiting for a client"
       client <- sAccept (sock :: TcpListenSocket IP4)
       putStrLn "Got a client"
       void $ forkIO (handleClient client)

     forever $ do
       threadDelay (secs 10)
       dumpStats (devStats dev)

secs :: Int -> Int
secs = (*1000000)

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
           then hPutStrLn stderr "Closing client"
           else do
             _ <- sWrite sock str
             loop
