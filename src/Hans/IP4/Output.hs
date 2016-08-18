{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Output (
    sendIP4, queueIP4,
    prepareIP4,
    primSendIP4,
    responder,

    -- * ICMP4 Messages
    queueIcmp4,
    portUnreachable,
  ) where

import Hans.Addr (IP4,pattern BroadcastIP4)
import Hans.Checksum (computeChecksum)
import Hans.Config (config,Config(..))
import Hans.Device
           (Device(..),DeviceConfig(..),DeviceStats(..),updateError,statTX
           ,ChecksumOffload(..),txOffload,deviceConfig)
import Hans.Ethernet
           ( Mac,sendEthernet,pattern ETYPE_IPV4, pattern ETYPE_ARP
           , pattern BroadcastMac)
import Hans.IP4.ArpTable
           (lookupEntry,resolveAddr,QueryResult(..),markUnreachable
           ,writeChanStrategy)
import Hans.IP4.Icmp4
           (Icmp4Packet(..),DestinationUnreachableCode(..),renderIcmp4Packet)
import Hans.IP4.Packet
import Hans.IP4.RoutingTable (Route(..),routeSource,routeNextHop)
import Hans.Lens
import Hans.Network.Types
import Hans.Serialize (runPutPacket)
import Hans.Threads (forkNamed)
import Hans.Types

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad (when,forever,unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Serialize.Put (putWord16be)



responder :: NetworkStack -> IO ()
responder ns = forever $
  do req <- BC.readChan (ip4ResponderQueue (view ip4State ns))
     case req of

       Send mbSrc dst df prot payload ->
         do _ <- sendIP4 ns mbSrc dst df prot payload
            return ()

       Finish dev mac frames ->
            sendIP4Frames dev mac frames


-- | Queue a message on the responder queue instead of attempting to send it
-- directly.
queueIP4 :: NetworkStack -> DeviceStats
         -> SendSource -> IP4 -> Bool -> NetworkProtocol -> L.ByteString
         -> IO ()
queueIP4 ns stats mbSrc dst df prot payload =
  do written <- BC.tryWriteChan (ip4ResponderQueue (view ip4State ns))
                    (Send mbSrc dst df prot payload)
     unless written (updateError statTX stats)


-- | Send an IP4 packet to the given destination. If it's not possible to find a
-- route to the destination, return False.
sendIP4 :: NetworkStack -> SendSource -> IP4
        -> Bool -> NetworkProtocol -> L.ByteString
        -> IO Bool

-- A special case for when the sender knows that this is the right device and
-- source address. The routing table is still queried to find the next hop, and
-- if the route found doesn't use the device provided, the packets aren't sent.
sendIP4 ns (SourceDev dev src) dst df prot payload =
  do mbRoute <- lookupRoute4 ns dst
     case mbRoute of
       Just (_,next,dev') | devName dev == devName dev' ->
         do primSendIP4 ns dev src dst next df prot payload
            return True

       _ ->
         do updateError statTX (devStats dev)
            return False

-- sending from a specific device
sendIP4 ns (SourceIP4 src) dst df prot payload =
  do mbRoute <- isLocalAddr ns src
     case mbRoute of
       Just route ->
         do primSendIP4 ns (routeDevice route) (routeSource route)
                dst (routeNextHop dst route) df prot payload
            return True

       Nothing ->
            return False

-- find the right path out
sendIP4 ns SourceAny dst df prot payload =
  do mbRoute <- lookupRoute4 ns dst
     case mbRoute of
       Just (src,next,dev) -> do primSendIP4 ns dev src dst next df prot payload
                                 return True
       Nothing             -> return False


prepareHeader :: NetworkStack -> IP4 -> IP4 -> Bool -> NetworkProtocol -> IO IP4Header
prepareHeader ns src dst df prot =
  do ident <- nextIdent ns
     return $! set ip4DontFragment df
               emptyIP4Header { ip4Ident      = ident
                              , ip4SourceAddr = src
                              , ip4DestAddr   = dst
                              , ip4Protocol   = prot
                              , ip4TimeToLive = cfgIP4InitialTTL (view config ns)
                              }


-- | Prepare IP4 fragments to be sent.
prepareIP4 :: NetworkStack -> Device -> IP4 -> IP4 -> Bool -> NetworkProtocol
           -> L.ByteString
           -> IO [L.ByteString]
prepareIP4 ns dev src dst df prot payload =
  do hdr <- prepareHeader ns src dst df prot

     let DeviceConfig { .. } = devConfig dev

     return $ [ renderIP4Packet (view txOffload dev) h p
              | (h,p) <- splitPacket (fromIntegral dcMtu) hdr payload ]


-- | Send an IP4 packet to the given destination. This assumes that routing has
-- already taken place, and that the source and destination addresses are
-- correct.
primSendIP4 :: NetworkStack -> Device -> IP4 -> IP4 -> IP4 -> Bool -> NetworkProtocol
             -> L.ByteString -> IO ()
primSendIP4 ns dev src dst next df prot payload
    -- when the source and next hop are the same, re-queue in the network stack
    -- after fragment reassembly
  | src == next =
    do hdr <- prepareHeader ns src dst df prot
       _   <- BC.tryWriteChan (nsInput ns) $! FromIP4 dev hdr (L.toStrict payload)
       -- don't write any stats for packets that skip the device layer
       return ()

    -- the packet is leaving the network stack so encode it and send
  | otherwise =
    do packets <- prepareIP4 ns dev src dst df prot payload
       arpOutgoing ns dev src next packets


-- | Retrieve the outgoing address for this IP4 packet, and send along all
-- fragments.
arpOutgoing :: NetworkStack -> Device -> IP4 -> IP4 -> [L.ByteString] -> IO ()
arpOutgoing _ dev _ BroadcastIP4 packets =
    sendIP4Frames dev BroadcastMac packets

arpOutgoing ns dev src next packets =
  do res <- resolveAddr (ip4ArpTable (view ip4State ns)) next queueSend
     case res of
       Known dstMac ->
         sendIP4Frames dev dstMac packets

       -- The mac wasn't present in the table. If this was the first request for
       -- this address, start a request thread.
       Unknown newRequest () ->
         when newRequest $ do _ <- forkNamed "arpRequestThread"
                                       (arpRequestThread ns dev src next)
                              return ()

  where

  queueSend =
    writeChanStrategy (Just (devStats dev)) mkFinish
        (ip4ResponderQueue (view ip4State ns))

  mkFinish mbMac =
    do dstMac <- mbMac
       return $! Finish dev dstMac packets


sendIP4Frames :: Device -> Mac -> [L.ByteString] -> IO ()
sendIP4Frames dev dstMac packets =
  mapM_ (sendEthernet dev dstMac ETYPE_IPV4) packets


-- | Make an Arp request for the given IP address, until the maximum retries
-- have been exhausted, or the entry made it into the table.
arpRequestThread :: NetworkStack -> Device -> IP4 -> IP4 -> IO ()
arpRequestThread ns dev src dst = loop 0
  where
  IP4State { ..} = view ip4State ns

  request = renderArpPacket ArpPacket { arpOper   = ArpRequest
                                      , arpSHA    = devMac dev
                                      , arpSPA    = src
                                      , arpTHA    = BroadcastMac
                                      , arpTPA    = dst
                                      }

  loop n =
    do sendEthernet dev BroadcastMac ETYPE_ARP request
       threadDelay ip4ArpRetryDelay

       mb <- lookupEntry ip4ArpTable dst
       case mb of
         Just{}                    -> return ()
         Nothing | n < ip4ArpRetry -> loop (n + 1)
                 | otherwise       -> markUnreachable ip4ArpTable dst


-- | The final step to render an IP header and its payload out as a lazy
-- 'ByteString'. Compute the checksum over the packet with its checksum zeroed,
-- then reconstruct a new lazy 'ByteString' that contains chunks from the old
-- header, and the new checksum.
renderIP4Packet :: ChecksumOffload -> IP4Header -> L.ByteString -> L.ByteString
renderIP4Packet ChecksumOffload { .. } hdr pkt
  | coIP4     = bytes `L.append` pkt
  | otherwise = withChecksum
  where

  pktlen    = L.length pkt

  bytes     = runPutPacket 20 40 pkt (putIP4Header hdr (fromIntegral pktlen))
  cs        = computeChecksum (L.take (L.length bytes - pktlen) bytes)

  beforeCS  = L.take 10 bytes
  afterCS   = L.drop 12 bytes
  csBytes   = runPutPacket 2 100 afterCS (putWord16be cs)

  withChecksum = beforeCS `L.append` csBytes


-- ICMP Messages ---------------------------------------------------------------

queueIcmp4 :: NetworkStack -> Device -> SendSource -> IP4 -> Icmp4Packet
           -> IO ()
queueIcmp4 ns dev src dst pkt =
  let msg = renderIcmp4Packet (view txOffload dev) pkt
      df  = fromIntegral (L.length msg) < dcMtu (view deviceConfig dev) - 20
   in queueIP4 ns (devStats dev) src dst df PROT_ICMP4 msg

-- | Emit a destination unreachable ICMP message. This will always be queued via
-- the responder queue, as it is most likely coming from the fast path. The
-- bytestring argument is assumed to be the original IP4 datagram, trimmed to
-- IP4 header + 8 bytes of data.
portUnreachable :: NetworkStack -> Device -> SendSource -> IP4 -> S.ByteString
                -> IO ()
portUnreachable ns dev src dst chunk =
  queueIcmp4 ns dev src dst (DestinationUnreachable PortUnreachable chunk)
