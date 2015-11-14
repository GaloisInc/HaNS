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
import Hans.IP4.State
import Hans.Monad

import           Control.Concurrent (takeMVar,forkIO,ThreadId,threadDelay)
import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad (when,forever,unless)
import qualified Data.ByteString.Lazy as L



responder :: IP4State -> IO ()
responder ip4 = forever $
  do req <- BC.readChan (ip4ResponderQueue ip4)
     case req of

       Send dst prot payload ->
         do _ <- sendIP4 ip4 dst prot payload
            return ()

       Finish dev mac frames ->
         mapM_ (sendEthernet dev mac ETYPE_IPV4) frames


-- | Queue a message on the responder queue instead of attempting to send it
-- directly.
queueIP4 :: IP4State -> DeviceStats -> IP4 -> IP4Protocol -> L.ByteString -> IO ()
queueIP4 ip4 stats dst prot payload =
  do written <- BC.tryWriteChan (ip4ResponderQueue ip4) (Send dst prot payload)
     unless written (updateError stats)


-- | Send an IP4 packet to the given destination. If it's not possible to find a
-- route to the destination, return False.
sendIP4 :: IP4State -> IP4 -> IP4Protocol -> L.ByteString -> IO Bool
sendIP4 ip4 dst prot payload =
  do mbRoute <- lookupRoute ip4 dst
     case mbRoute of
       Just (src,next,dev) -> do primSendIP4 ip4 dev src dst next prot payload
                                 return True
       Nothing             -> return False



-- | Prepare IP4 fragments to be sent.
prepareIP4 :: IP4State -> Device -> IP4 -> IP4 -> IP4Protocol -> L.ByteString
           -> IO [L.ByteString]
prepareIP4 ip4 dev src dst prot payload =
  do ident <- nextRandom ip4

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
primSendIP4 :: IP4State -> Device -> IP4 -> IP4 -> IP4 -> IP4Protocol
             -> L.ByteString -> IO ()
primSendIP4 ip4 dev src dst next prot payload =
  do packets <- prepareIP4 ip4 dev src dst prot payload
     arpOutgoing ip4 dev src next packets


-- | Retrieve the outgoing address for this IP4 packet, and send along all
-- fragments.
arpOutgoing :: IP4State -> Device -> IP4 -> IP4 -> [L.ByteString] -> IO ()
arpOutgoing ip4 dev src next packets =
  do res <- resolveAddr (ip4ArpTable ip4) next queueSend
     case res of
       Known dstMac ->
         mapM_ (sendEthernet dev dstMac ETYPE_IPV4) packets

       -- The mac wasn't present in the table. If this was the first request for
       -- this address, start a request thread.
       Unknown newRequest () ->
         when newRequest $ do _ <- forkIO (arpRequestThread ip4 dev src next)
                              return ()

  where

  queueSend =
    writeChanStrategy (Just (devStats dev)) mkFinish (ip4ResponderQueue ip4)

  mkFinish mbMac =
    do dstMac <- mbMac
       return $! Finish dev dstMac packets


-- | Make an Arp request for the given IP address, until the maximum retries
-- have been exhausted, or the entry made it into the table.
arpRequestThread :: IP4State -> Device -> IP4 -> IP4 -> IO ()
arpRequestThread IP4State { .. } dev src dst = loop 0
  where

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
