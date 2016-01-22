{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Input where

import Hans.Device (Device(..))
import Hans.Ethernet
import Hans.IP4.Input (processArp,processIP4,handleIP4)
import Hans.Monad (Hans,runHans,dropPacket,io,escape,decode')
import Hans.Types (NetworkStack(..),InputPacket(..))

import           Control.Concurrent.BoundedChan (readChan)
import           Control.Monad (unless)
import qualified Data.ByteString as S


-- Incoming Packets ------------------------------------------------------------

-- | Handle incoming packets.
processPackets :: NetworkStack -> IO ()
processPackets ns = runHans $
  do input <- io (readChan (nsInput ns))
     case input of
       FromDevice dev pkt   -> processEthernet ns dev pkt
       FromIP4 dev hdr body -> handleIP4 ns dev Nothing hdr body


processEthernet :: NetworkStack -> Device -> S.ByteString -> Hans ()
processEthernet ns dev bytes =
  do (hdr,payload) <- decode' (devStats dev) getEthernetHeader bytes

     -- XXX at some point, we should extend this to support multicast
     let validFrame = eDest hdr == BroadcastMac
                   || eDest hdr == devMac dev

     -- XXX should we increment a stat here?
     unless validFrame escape

     case eType hdr of

       ETYPE_IPV4 ->
         processIP4 ns dev payload

       ETYPE_ARP ->
         processArp ns dev payload

       _ ->
         dropPacket (devStats dev)
