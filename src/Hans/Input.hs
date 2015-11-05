{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Input where

import Hans.Arp
import Hans.Device (InputPacket(..))
import Hans.Ethernet
import Hans.Monad (runHans,dropPacket,io)
import Hans.Types (NetworkStack(..))

import Control.Concurrent.BoundedChan (readChan)


-- Incoming Packets ------------------------------------------------------------

-- | Handle incoming packets.
processPackets :: NetworkStack -> IO ()
processPackets NetworkStack { .. } = runHans $
  do input               <- io (readChan nsInput)
     (stats,hdr,payload) <- decodeEthernet input

     case eType hdr of

       ETYPE_IPV4 ->
         dropPacket stats

       ETYPE_ARP ->
         processArp nsConfig nsArpState (ipDevice input) payload

       ETYPE_IPV6 ->
         dropPacket stats

       _ ->
         dropPacket stats
