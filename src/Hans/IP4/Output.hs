{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Output (
    sendIP4, queueIP4,
    prepareIP4,
    primSendIP4,
    responder,
  ) where

import Hans.Device (Device(..),DeviceConfig(..),DeviceStats(..),updateError)
import Hans.Ethernet
           ( Mac,sendEthernet,pattern ETYPE_IPV4, pattern ETYPE_ARP
           , pattern BroadcastMac)
import Hans.IP4.ArpTable
           (lookupEntry,resolveAddr,QueryResult(..),markUnreachable
           ,writeChanStrategy)
import Hans.IP4.Icmp4 (Icmp4Packet)
import Hans.IP4.Packet
import Hans.IP4.RoutingTable (Route(..),routeSource,routeNextHop)
import Hans.Monad
import Hans.Types

import           Control.Concurrent (takeMVar,forkIO,ThreadId,threadDelay)
import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad (when,forever,unless)
import qualified Data.ByteString.Lazy as L



responder :: NetworkStack -> IO ()
responder ns = forever $
  do req <- BC.readChan (ip4ResponderQueue (getIP4State ns))
     case req of

       Send mbSrc dst prot payload ->
         do _ <- sendIP4 ns mbSrc dst prot payload
            return ()

       Finish dev mac frames ->
            sendIP4Frames dev mac frames


-- | Queue a message on the responder queue instead of attempting to send it
-- directly.
queueIP4 :: NetworkStack -> DeviceStats
         -> Maybe IP4 -> IP4 -> IP4Protocol -> L.ByteString
         -> IO ()
queueIP4 ns stats mbSrc dst prot payload =
  do written <- BC.tryWriteChan (ip4ResponderQueue (getIP4State ns))
                    (Send mbSrc dst prot payload)
     unless written (updateError stats)


-- | Send an IP4 packet to the given destination. If it's not possible to find a
-- route to the destination, return False.
sendIP4 :: NetworkStack -> Maybe IP4 -> IP4 -> IP4Protocol -> L.ByteString
        -> IO Bool

-- sending from a specific device
sendIP4 ns (Just src) dst prot payload =
  do mbRoute <- isLocalAddr ns src
     case mbRoute of
       Just route ->
         do primSendIP4 ns (routeDevice route) (routeSource route)
                dst (routeNextHop dst route) prot payload
            return True

       Nothing ->
            return False

-- find the right path out
sendIP4 ns Nothing dst prot payload =
  do mbRoute <- lookupRoute ns dst
     case mbRoute of
       Just (src,next,dev) -> do primSendIP4 ns dev src dst next prot payload
                                 return True
       Nothing             -> return False



-- | Prepare IP4 fragments to be sent.
prepareIP4 :: NetworkStack -> Device -> IP4 -> IP4 -> IP4Protocol -> L.ByteString
           -> IO [L.ByteString]
prepareIP4 ns dev src dst prot payload =
  do ident <- nextIdent ns

     let hdr = emptyIP4Header { ip4Ident      = ident
                              , ip4SourceAddr = src
                              , ip4DestAddr   = dst
                              , ip4Protocol   = prot
                              }

     let DeviceConfig { .. } = devConfig dev

     return $ [ renderIP4Packet (not dcChecksumOffload) h p
              | (h,p) <- splitPacket (fromIntegral dcMtu) hdr payload ]


-- | Send an IP4 packet to the given destination. This assumes that routing has
-- already taken place, and that the source and destination addresses are
-- correct.
--
-- XXX: if the destination is an address managed by us, re-inject the packet
-- into the network stack without rendering it.
primSendIP4 :: NetworkStack -> Device -> IP4 -> IP4 -> IP4 -> IP4Protocol
             -> L.ByteString -> IO ()
primSendIP4 ns dev src dst next prot payload =
  do packets <- prepareIP4 ns dev src dst prot payload
     arpOutgoing ns dev src next packets


-- | Retrieve the outgoing address for this IP4 packet, and send along all
-- fragments.
arpOutgoing :: NetworkStack -> Device -> IP4 -> IP4 -> [L.ByteString] -> IO ()
arpOutgoing ns dev src next packets
  | next == broadcastIP4 =
    sendIP4Frames dev BroadcastMac packets

  | otherwise =
    do res <- resolveAddr (ip4ArpTable (getIP4State ns)) next queueSend
       case res of
         Known dstMac ->
           sendIP4Frames dev dstMac packets

         -- The mac wasn't present in the table. If this was the first request for
         -- this address, start a request thread.
         Unknown newRequest () ->
           when newRequest $ do _ <- forkIO (arpRequestThread ns dev src next)
                                return ()

  where

  queueSend =
    writeChanStrategy (Just (devStats dev)) mkFinish
        (ip4ResponderQueue (getIP4State ns))

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
  IP4State { ..} = getIP4State ns

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
