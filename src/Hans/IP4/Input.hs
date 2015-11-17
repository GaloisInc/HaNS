{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Input (
    processArp,
    processIP4,
    handleIP4,
  ) where

import Hans.Checksum (computeChecksum)
import Hans.Device (Device(..),DeviceConfig(..))
import Hans.Ethernet (Mac,pattern ETYPE_ARP,sendEthernet)
import Hans.IP4.ArpTable (addEntry,lookupEntry)
import Hans.IP4.Fragments (processFragment)
import Hans.IP4.Icmp4 (Icmp4Packet(..),getIcmp4Packet,renderIcmp4Packet)
import Hans.IP4.Output (queueIP4)
import Hans.IP4.Packet
import Hans.IP4.RoutingTable (Route(..))
import Hans.Monad (Hans,io,dropPacket,escape,decode,decode')
import Hans.Serialize (runPutPacket)
import Hans.Types
import Hans.UDP.Input (processUDP)

import           Control.Monad (when,unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Arp Processing --------------------------------------------------------------

-- | Handle incoming Arp packets.
processArp :: NetworkStack -> Device -> S.ByteString -> Hans ()
processArp ns dev payload =
  do ArpPacket { .. } <- decode (devStats dev) getArpPacket payload

     -- if the entry already exists in the arp table, update it
     merge <- io (updateEntry ns arpSHA arpSPA)

     -- are we the target of the request?
     mb   <- io (isLocalAddr ns arpTPA)
     dev' <- case mb of
               Just route -> return (routeDevice route)
               Nothing    -> escape
     let lha = devMac dev'

     -- add the entry if it didn't already exist
     unless merge (io (addEntry (ip4ArpTable (getIP4State ns)) arpSPA arpSHA))

     -- respond if the packet was a who-has request for our mac
     when (arpOper == ArpRequest)
       $ io
       $ sendEthernet dev' arpSHA ETYPE_ARP
       $ runPutPacket 28 100 L.empty
       $ putArpPacket ArpPacket { arpSHA  = lha,    arpSPA = arpTPA
                                , arpTHA  = arpSHA, arpTPA = arpSPA
                                , arpOper = ArpReply }


-- | Update an entry in the arp table, if it exists already.
updateEntry :: NetworkStack -> Mac -> IP4 -> IO Bool
updateEntry ns sha spa =
  do mb <- lookupEntry (ip4ArpTable (getIP4State ns)) spa
     case mb of

       Just _  -> do addEntry (ip4ArpTable (getIP4State ns)) spa sha
                     return True

       Nothing -> return False


-- IP4 Processing --------------------------------------------------------------

-- | Process a packet that has arrived from a device.
processIP4 :: NetworkStack -> Device -> S.ByteString -> Hans ()
processIP4 ns dev payload =
  do ((hdr,hdrLen,bodyLen),body) <- decode' (devStats dev) getIP4Packet payload

     -- only validate the checkum if the device hasn't done that already
     let packetValid = dcChecksumOffload (devConfig dev)
                    || 0 == computeChecksum (S.take hdrLen payload)
     unless packetValid (dropPacket (devStats dev))

     -- Drop packets that weren't destined for an address that this device
     -- holds.
     -- XXX: if we ever want to use HaNS as a router, this would be where IP
     -- routing would happen
     checkDestination ns dev (ip4DestAddr hdr)

     handleIP4 ns dev hdr (S.take bodyLen body)


-- | The processing stage after the packet has been decoded and validated. It's
-- exposed here so that routing to an address that's managed by the network
-- stack can skip the device layer.
handleIP4 :: NetworkStack -> Device -> IP4Header -> S.ByteString -> Hans ()
handleIP4 ns dev hdr body =
  do (IP4Header { .. },body') <-
         processFragment (ip4Fragments (getIP4State ns)) hdr body

     case ip4Protocol of
       IP4_PROT_ICMP -> processICMP ns dev ip4SourceAddr ip4DestAddr body'
       IP4_PROT_UDP  -> processUDP  ns dev ip4SourceAddr ip4DestAddr body'
       _             -> dropPacket (devStats dev)


-- | Validate the destination of this packet.
checkDestination :: NetworkStack -> Device -> IP4 -> Hans ()
checkDestination ns dev dest

    -- always accept broadcast messages
  | dest == broadcastIP4 = return ()

    -- require that the input device has the destination address
  | otherwise =
    do mb <- io (isLocalAddr ns dest)
       case mb of
         Just Route { .. } | devName routeDevice == devName dev -> return ()
         _                                                      -> escape




-- ICMP Processing -------------------------------------------------------------

-- | Process incoming ICMP packets.
processICMP :: NetworkStack -> Device -> IP4 -> IP4 -> S.ByteString -> Hans ()

processICMP ns dev src dst body =
  do let packetValid = dcChecksumOffload (devConfig dev)
                    || 0 == computeChecksum body

     unless packetValid (dropPacket (devStats dev))

     msg <- decode (devStats dev) getIcmp4Packet body

     case msg of

       -- XXX: use the stats and config for the device that the message arrived
       -- on. As it's probably going out the same device, this seems OK, but it
       -- would be nice to confirm that.
       Echo ident seqNum bytes ->
         do let packet = renderIcmp4Packet
                             (not (dcChecksumOffload (devConfig dev)))
                             (EchoReply ident seqNum bytes)
            io (queueIP4 ns (devStats dev) (SourceIP4 dst) src IP4_PROT_ICMP packet)
            escape


       -- Drop all other messages for now
       _ -> escape
