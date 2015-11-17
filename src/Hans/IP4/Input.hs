{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Input where

import Hans.Checksum (computeChecksum)
import Hans.Config (Config(..))
import Hans.Device (Device(..),DeviceConfig(..))
import Hans.Ethernet (Mac,pattern ETYPE_ARP,sendEthernet)
import Hans.IP4.ArpTable (addEntry,lookupEntry)
import Hans.IP4.Fragments (processFragment)
import Hans.IP4.Icmp4 (Icmp4Packet(..),getIcmp4Packet,renderIcmp4Packet)
import Hans.IP4.Output (queueIP4)
import Hans.IP4.Packet
import Hans.IP4.RoutingTable (isLocal)
import Hans.IP4.State (IP4State(..))
import Hans.Monad (Hans,io,dropPacket,escape,decode,decode')
import Hans.Serialize (runPutPacket)

import           Control.Monad (when,unless)
import           Data.IORef (readIORef)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Arp Processing --------------------------------------------------------------

-- | Handle incoming Arp packets.
processArp :: Config -> IP4State -> Device -> S.ByteString -> Hans ()
processArp cfg ip4 dev payload =
  do ArpPacket { .. } <- decode (devStats dev) getArpPacket payload

     -- if the entry already exists in the arp table, update it
     merge <- io (updateEntry cfg ip4 arpSHA arpSPA)

     -- are we the target of the request?
     dev' <- guardLocalAddress ip4 arpTPA
     let lha = devMac dev'

     -- add the entry if it didn't already exist
     unless merge (io (addEntry (ip4ArpTable ip4) arpSPA arpSHA))

     -- respond if the packet was a who-has request for our mac
     when (arpOper == ArpRequest)
       $ io
       $ sendEthernet dev' arpSHA ETYPE_ARP
       $ runPutPacket 28 100 L.empty
       $ putArpPacket ArpPacket { arpSHA  = lha,    arpSPA = arpTPA
                                , arpTHA  = arpSHA, arpTPA = arpSPA
                                , arpOper = ArpReply }


-- | Update an entry in the arp table, if it exists already.
updateEntry :: Config -> IP4State -> Mac -> IP4 -> IO Bool
updateEntry Config { .. } IP4State { .. } sha spa =
  do mb <- lookupEntry ip4ArpTable spa
     case mb of

       Just _  -> do addEntry ip4ArpTable spa sha
                     return True

       Nothing -> return False


-- | Require that the address given belongs to one of the managed devices. If
-- the address is unknown, invoke the escape continuation to abort packet
-- processing.
guardLocalAddress :: IP4State -> IP4 -> Hans Device
guardLocalAddress IP4State { .. } dst =
  do rt <- io (readIORef ip4Routes)
     case isLocal dst rt of
       Just dev -> return dev
       Nothing  -> escape


-- IP4 Processing --------------------------------------------------------------

processIP4 :: Config -> IP4State -> Device -> S.ByteString -> Hans ()
processIP4 _cfg ip4 dev payload =
  do ((hdr,hdrLen,bodyLen),body) <- decode' (devStats dev) getIP4Packet payload

     -- only validate the checkum if the device hasn't done that already
     let packetValid = dcChecksumOffload (devConfig dev)
                    || 0 == computeChecksum (S.take hdrLen payload)
     unless packetValid (dropPacket (devStats dev))

     (hdr',body') <- processFragment (ip4Fragments ip4) hdr (S.take bodyLen body)

     -- was this packet destined for a local address?
     routeLocal ip4 dev hdr' body'
       -- XXX: as routeLocal can fail, add in support for conditional routing
       -- based on network stack config.
       -- XXX: do we ever want to support using HaNS as a router?


-- | Make sure that the destination of this packet is a device that we manage,
-- then pass it on to the appropriate layer for further processing.
routeLocal :: IP4State -> Device -> IP4Header -> S.ByteString -> Hans ()
routeLocal ip4 inDev hdr body =
  do _ <- guardLocalAddress ip4 (ip4DestAddr hdr)

     case ip4Protocol hdr of

       IP4_PROT_ICMP -> processICMP ip4 inDev hdr body

       _ ->
         dropPacket (devStats inDev)


-- ICMP Processing -------------------------------------------------------------

-- | Process incoming ICMP packets.
processICMP :: IP4State -> Device -> IP4Header -> S.ByteString -> Hans ()

processICMP ip4 dev hdr body =
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
            io (queueIP4 ip4 (devStats dev) (ip4SourceAddr hdr) IP4_PROT_ICMP packet)
            escape


       -- Drop all other messages for now
       _ -> escape
