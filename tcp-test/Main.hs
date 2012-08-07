module Main where

import Hans.Address
import Hans.Address.Mac
import Hans.Address.IP4
import Hans.Device.Tap
import Hans.NetworkStack

import Control.Concurrent (threadDelay,forkIO,killThread,myThreadId)
import Control.Monad (forever,when)
import qualified Data.ByteString.Lazy as L


localAddr :: IP4
localAddr  = IP4 192 168 90 2

main :: IO ()
main  = do
  ns  <- newNetworkStack
  mac <- initEthernetDevice ns
  deviceUp ns mac
  setAddress mac ns
  putStrLn "Network stack running..."

  sock <- listen ns localAddr 9001

  forever $ do
    putStrLn "accepting"
    client <- accept sock
    _ <- forkIO $ do
      putStrLn ("Got one: " ++ show (sockRemoteHost client))
      forever $ do
        buf <- recvBytes client 512
        when (L.null buf) $ do
          putStrLn "Client closed connection"
          close client
          killThread =<< myThreadId
        _ <- sendBytes client buf
        return ()
    return ()

message = L.pack (map (toEnum . fromEnum) "Hello, world\n")

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000 * 1000)

initEthernetDevice :: NetworkStack -> IO Mac
initEthernetDevice ns = do
  let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x56
  Just dev <- openTapDevice "tap0"
  addDevice ns mac (tapSend dev) (tapReceiveLoop dev)
  return mac

setAddress :: Mac -> NetworkStack -> IO ()
setAddress mac ns = do
  addIP4Addr ns (localAddr `withMask` 24) mac 1500
  routeVia ns (IP4 0 0 0 0 `withMask` 0) (IP4 192 168 90 1)
