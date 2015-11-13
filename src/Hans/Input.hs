{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Input where

import Hans.Device (InputPacket(..))
import Hans.Ethernet
import Hans.IP4.Input (processArp,processIP4)
import Hans.Monad (runHans,dropPacket,io)
import Hans.Types (NetworkStack(..))

import Control.Concurrent.BoundedChan (readChan)


-- Incoming Packets ------------------------------------------------------------

-- | Handle incoming packets.
processPackets :: NetworkStack -> IO ()
processPackets NetworkStack { .. } = runHans $
  do input               <- io (readChan nsInput)
     (stats,hdr,payload) <- decodeEthernet input
     io (print payload)

     case eType hdr of

       ETYPE_IPV4 ->
         processIP4 nsConfig nsIP4State (ipDevice input) payload

       ETYPE_ARP ->
         processArp nsConfig nsIP4State (ipDevice input) payload

       ETYPE_IPV6 ->
         dropPacket stats

       _ ->
         dropPacket stats
