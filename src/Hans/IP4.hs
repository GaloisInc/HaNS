{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4 (
    module Exports,
    module Hans.IP4
  ) where

import Hans.Arp.Types (ArpState(..))
import Hans.Checksum (computeChecksum)
import Hans.Device
import Hans.IP4.Types as Exports
import Hans.Monad (Hans,decode',dropPacket,io)

import           Control.Monad (unless)
import qualified Data.ByteString as S
import           Data.IORef(readIORef)


processIP4 :: ArpState -> IP4State -> Device -> S.ByteString -> Hans ()
processIP4 arp IP4State dev payload =
  do ((hdr,hdrLen,_),body) <- decode' (devStats dev) getIP4Packet payload

     -- only validate the checkum if the device hasn't done that already
     let packetValid = dcChecksumOffload (devConfig dev)
                    || 0 == computeChecksum 0 (S.take hdrLen payload)
     unless packetValid (dropPacket (devStats dev))

     -- was this packet destined for a local address?
     routeLocal arp dev hdr body
       -- XXX: as routeLocal can fail, add in support for conditional routing
       -- based on network stack config.


-- | Make sure that the destination of this packet is a device that we manage,
-- then pass it on to the appropriate layer for further processing.
routeLocal :: ArpState -> Device -> IP4Header -> S.ByteString -> Hans ()
routeLocal ArpState { .. } dev IP4Header { .. } _body =
  do addrs <- io (readIORef arpAddrs)
     requireLocal addrs

     case ip4Protocol of

       _ ->
         dropPacket (devStats dev)

  where

  requireLocal ((addr,_) : rest)
    | addr == ip4DestAddr = return ()
    | otherwise           = requireLocal rest

  requireLocal [] = dropPacket (devStats dev)
