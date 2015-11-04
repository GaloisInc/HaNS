module Main where

import Hans

import qualified Data.ByteString.Char8 as S8
import           System.Environment (getArgs)


main :: IO ()
main  =
  do args <- getArgs
     name <- case args of
               [name] -> return (S8.pack name)
               _      -> fail "Expected a device name"

     ns  <- newNetworkStack defaultConfig
     dev <- addDevice name defaultDeviceConfig ns

     -- start receiving data
     startDevice dev

     processPackets ns
