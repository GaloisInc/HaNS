{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Input (
    processArp,
    processIP4,
    handleIP4,
  ) where

import Hans.Checksum (computeChecksum)
import Hans.Device (Device(..),ChecksumOffload(..),rxOffload)
import Hans.Ethernet (Mac,pattern ETYPE_ARP,sendEthernet)
import Hans.IP4.ArpTable (addEntry,lookupEntry)
import Hans.IP4.Fragments (processFragment)
import Hans.IP4.Icmp4 (Icmp4Packet(..),getIcmp4Packet)
import Hans.IP4.Output (queueIcmp4,portUnreachable)
import Hans.IP4.Packet
import Hans.IP4.RoutingTable (Route(..))
import Hans.Lens (view)
import Hans.Monad (Hans,io,dropPacket,escape,decode,decode')
import Hans.Network.Types
import Hans.Serialize (runPutPacket)
import Hans.Types
import Hans.Udp.Input (processUdp)
import Hans.Tcp.Input (processTcp)

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
     unless merge (io (addEntry (ip4ArpTable (view ip4State ns)) arpSPA arpSHA))

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
  do mb <- lookupEntry (ip4ArpTable (view ip4State ns)) spa
     case mb of

       Just _  -> do addEntry (ip4ArpTable (view ip4State ns)) spa sha
                     return True

       Nothing -> return False


-- IP4 Processing --------------------------------------------------------------

-- | Process a packet that has arrived from a device.
processIP4 :: NetworkStack -> Device -> S.ByteString -> Hans ()
processIP4 ns dev payload =
  do ((hdr,hdrLen,bodyLen),body) <- decode' (devStats dev) getIP4Packet payload

     -- only validate the checkum if the device hasn't done that already
     let packetValid = coIP4 (view rxOffload dev)
                    || 0 == computeChecksum (S.take hdrLen payload)
     unless packetValid (dropPacket (devStats dev))

     -- Drop packets that weren't destined for an address that this device
     -- holds.
     -- XXX: if we ever want to use HaNS as a router, this would be where IP
     -- routing would happen
     checkDestination ns dev (ip4DestAddr hdr)

     handleIP4 ns dev (Just (hdrLen,payload)) hdr (S.take bodyLen body)


-- | The processing stage after the packet has been decoded and validated. It's
-- exposed here so that routing to an address that's managed by the network
-- stack can skip the device layer.
handleIP4 :: NetworkStack -> Device -> Maybe (Int,S.ByteString)
          -> IP4Header -> S.ByteString -> Hans ()
handleIP4 ns dev mbOrig hdr body =
  do (IP4Header { .. },body') <-
         processFragment (ip4Fragments (view ip4State ns)) hdr body

     case ip4Protocol of
       PROT_ICMP4 -> processICMP ns dev ip4SourceAddr ip4DestAddr body'

       PROT_UDP   ->
         do routed <- processUdp ns dev ip4SourceAddr ip4DestAddr body'
            case (routed,mbOrig) of
              -- when we failed to route the datagram, and have the original
              -- message handy, send a portUnreachable message.
              (False,Just(ihl,orig)) -> io
                $ portUnreachable ns dev (SourceIP4 ip4DestAddr) ip4SourceAddr
                $ S.take (ihl + 8) orig

              -- otherwise, the packet was routed from an internal device, or a
              -- destination actually existed.
              _ -> return ()

       PROT_TCP   -> processTcp  ns dev ip4SourceAddr ip4DestAddr body'
       _          -> dropPacket (devStats dev)


-- | Validate the destination of this packet.
checkDestination :: NetworkStack -> Device -> IP4 -> Hans ()

-- always accept broadcast messages
checkDestination _ _ BroadcastIP4 = return ()

-- require that the input device has the destination address
checkDestination ns dev dest =
  do mb <- io (isLocalAddr ns dest)
     case mb of
       Just Route { .. }
         | routeDevice == dev -> return ()

           -- A route was found, and it didn't involve the device the packet
           -- arrived on
         | otherwise          -> escape

       -- No route was found. Check to see if there are any routes for this
       -- device, and forward the packet on if there are none (in order to
       -- support things like unicast DHCP).
       Nothing ->
         do routes <- io (routesForDev ns dev)
            unless (null routes) escape




-- ICMP Processing -------------------------------------------------------------

-- | Process incoming ICMP packets.
processICMP :: NetworkStack -> Device -> IP4 -> IP4 -> S.ByteString -> Hans ()

processICMP ns dev src dst body =
  do let packetValid = coIcmp4 (view rxOffload dev) || 0 == computeChecksum body

     unless packetValid (dropPacket (devStats dev))

     msg <- decode (devStats dev) getIcmp4Packet body

     case msg of

       -- XXX: use the stats and config for the device that the message arrived
       -- on. As it's probably going out the same device, this seems OK, but it
       -- would be nice to confirm that.
       Echo ident seqNum bytes ->
         do io (queueIcmp4 ns dev (SourceIP4 dst) src (EchoReply ident seqNum bytes))
            escape


       -- Drop all other messages for now
       _ -> escape
