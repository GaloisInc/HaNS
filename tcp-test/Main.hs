module Main where

import Hans.Address
import Hans.Address.Mac
import Hans.Address.IP4
import Hans.Device.Tap
import Hans.NetworkStack

import Control.Concurrent (threadDelay)
import Control.Monad (forever)


main :: IO ()
main  = do
  ns  <- newNetworkStack
  mac <- initEthernetDevice ns
  deviceUp ns mac
  setAddress mac ns
  putStrLn "Network stack running..."
  forever (threadDelay maxBound)

initEthernetDevice :: NetworkStack -> IO Mac
initEthernetDevice ns = do
  let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x56
  Just dev <- openTapDevice "tap0"
  addDevice ns mac (tapSend dev) (tapReceiveLoop dev)
  return mac

setAddress :: Mac -> NetworkStack -> IO ()
setAddress mac ns = do
  addIP4Addr ns (IP4 192 168 90 2 `withMask` 24) mac 1500
  routeVia ns (IP4 0 0 0 0 `withMask` 0) (IP4 192 168 50 1)
