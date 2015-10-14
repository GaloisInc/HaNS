{-# LANGUAGE BangPatterns #-}

module Hans.Input where

import Hans.Device
import Hans.Monad
import Hans.Queue

import Data.Array (Array,listArray,(!))


-- Input Queue -----------------------------------------------------------------

newtype PacketReader =
  PacketReader { readPacket :: STM (Device,S.ByteString,PacketReader) }

-- | Read an incoming packet from one of the enabled devices. Device input
-- queues are polled in round-robin order.
roundRobinReader :: [Device] -> PacketReader
roundRobinReader devList = PacketReader (go numDevs 0)
  where
  numDevs = length devList
  devs    = listArray (0,numDevs - 1) devList

  nextDev devId = (devId + 1) `mod` numDevs

  go n !devId
    | n <= 0    = retry
    | otherwise = getPacket devId `orElse` go (n - 1) (nextDevId devId)

  getPacket devId =
    do let dev = devs ! devId
       pkt <- dequeue dev
       return (dev,pkt,PacketReader (go numDevs $! nextDevId devId))

-- | Handle incoming packets.
processPackets :: [Device] -> Hans ()
processPackets devList = runHans (loop (roundRobinReader devList))
  where
  loop devs =
    do (dev,pkt,reader') <- stm (readPacket devs)
       io (print pkt)

       -- Any packet processing at this point should be run under `setEscape' so
       -- that we don't restart the whole loop from scratch, and lose the
       -- context for the round-robin scheduler.

       loop reader'
