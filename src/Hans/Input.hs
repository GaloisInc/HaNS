{-# LANGUAGE RecordWildCards #-}

module Hans.Input where

import Hans.Monad
import Hans.Queue

import qualified Data.ByteString as S


-- Incoming Packets ------------------------------------------------------------

-- | Handle incoming packets.
processPackets :: Queue S.ByteString -> IO ()
processPackets input = runHans $
  do pkt <- stm (dequeue input)
     io (print pkt)
