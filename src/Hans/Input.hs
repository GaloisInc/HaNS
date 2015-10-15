{-# LANGUAGE BangPatterns #-}

module Hans.Input where

import Hans.Device
import Hans.Monad
import Hans.Queue

import qualified Data.ByteString as S


-- Incoming Packets ------------------------------------------------------------

-- | Handle incoming packets.
processPackets :: Queue (Device,S.ByteString) -> IO ()
processPackets inputQueue = runHans $
  do (dev,pkt) <- stm (dequeue inputQueue)
     io (print pkt)
