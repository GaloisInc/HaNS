{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Input where

import Hans.Arp
import Hans.Device (InputPacket(..))
import Hans.Ethernet
import Hans.Monad (runHans,dropPacket,stm)
import Hans.Queue (dequeue)
import Hans.Types (NetworkStack(..))


-- Incoming Packets ------------------------------------------------------------

-- | Handle incoming packets.
processPackets :: NetworkStack -> IO ()
processPackets NetworkStack { .. } = runHans $
  do input               <- stm (dequeue nsInput)
     (stats,hdr,payload) <- decodeEthernet input

     case eType hdr of

       ETYPE_IPV4 ->
         dropPacket stats

       ETYPE_ARP ->
         processArp nsArpState (ipDevice input) hdr payload

       ETYPE_IPV6 ->
         dropPacket stats

       _ ->
         dropPacket stats
