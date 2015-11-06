{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Input where

import Hans.IP4.ArpTable (addEntry,lookupEntry)
import Hans.Checksum (computeChecksum)
import Hans.Config (Config(..))
import Hans.Device (Device(..),DeviceConfig(..))
import Hans.Ethernet (Mac,pattern ETYPE_ARP,sendEthernet)
import Hans.IP4.Packet
import Hans.IP4.State (IP4State(..))
import Hans.Monad (Hans,io,dropPacket,escape,decode,decode')
import Hans.Serialize (runPutPacket)

import           Control.Monad (when,unless)
import           Data.IORef (readIORef)
import           Data.List (find)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Arp Processing --------------------------------------------------------------

-- | Handle incoming Arp packets.
processArp :: Config -> IP4State -> Device -> S.ByteString -> Hans ()
processArp cfg ip4 dev payload =
  do ArpPacket { .. } <- decode (devStats dev) getArpPacket payload

     merge <- io (updateEntry cfg ip4 arpSHA arpSPA)

     -- are we the target of the request?
     dev' <- guardLocalAddress ip4 arpTPA
     let lha = devMac dev'

     -- add the entry if it didn't already exist
     unless merge $ io
                  $ addEntry arpSPA arpSHA (cfgArpTableLifetime cfg)
                  $ ip4ArpTable ip4

     -- respond if the packet was a who-has request for our mac
     when (arpOper == ArpRequest)
       $ sendEthernet dev' arpSHA ETYPE_ARP
       $ runPutPacket 28 100 L.empty
       $ putArpPacket ArpPacket { arpSHA  = lha,    arpSPA = arpTPA
                                , arpTHA  = arpSHA, arpTPA = arpSPA
                                , arpOper = ArpReply }


-- | Update an entry in the arp table, if it exists already.
updateEntry :: Config -> IP4State -> Mac -> IP4 -> IO Bool
updateEntry Config { .. } IP4State { .. } sha spa =
  do mb <- lookupEntry spa ip4ArpTable
     case mb of

       Just _  -> do addEntry spa sha cfgArpTableLifetime ip4ArpTable
                     return True

       Nothing -> return False


-- | Require that the address given belongs to one of the managed devices. If
-- the address is unknown, invoke the escape continuation to abort packet
-- processing.
guardLocalAddress :: IP4State -> IP4 -> Hans Device
guardLocalAddress IP4State { .. } dst =
  do addrs <- io (readIORef ip4Addrs)
     case find hasDst addrs of
       Just (_,dev) -> return dev
       Nothing      -> escape
  where

  hasDst (addr,_) = addr == dst


-- IP4 Processing --------------------------------------------------------------

processIP4 :: Config -> IP4State -> Device -> S.ByteString -> Hans ()
processIP4 _ ip4 dev payload =
  do ((hdr,hdrLen,_),body) <- decode' (devStats dev) getIP4Packet payload

     -- only validate the checkum if the device hasn't done that already
     let packetValid = dcChecksumOffload (devConfig dev)
                    || 0 == computeChecksum 0 (S.take hdrLen payload)
     unless packetValid (dropPacket (devStats dev))

     -- was this packet destined for a local address?
     routeLocal ip4 dev hdr body
       -- XXX: as routeLocal can fail, add in support for conditional routing
       -- based on network stack config.


-- | Make sure that the destination of this packet is a device that we manage,
-- then pass it on to the appropriate layer for further processing.
routeLocal :: IP4State -> Device -> IP4Header -> S.ByteString -> Hans ()
routeLocal ip4 dev IP4Header { .. } _body =
  do _ <- guardLocalAddress ip4 ip4DestAddr

     case ip4Protocol of

       _ ->
         dropPacket (devStats dev)


-- ICMP Processing -------------------------------------------------------------
