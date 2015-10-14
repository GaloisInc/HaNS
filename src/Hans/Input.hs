module Hans.Input where

import Hans.Monad

import Control.Concurrent.STM (atomically,TQueue,takeTQueue)


processPackets :: TQueue (Device,S.ByteString) -> Hans ()
processPackets input =
  do (dev,pkt) <- stm (takeTQueue input)
