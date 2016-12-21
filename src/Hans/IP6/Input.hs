{-# LANGUAGE RecordWildCards #-}
module Hans.IP6.Input(
         processIP6
       , handleIP6
       )
 where

import           Control.Monad(unless)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Hans.Addr
import           Hans.Checksum(computeChecksum)
import           Hans.Device(Device(..),ChecksumOffload(..),rxOffload)
import           Hans.IP6.Packet(IP6Header(..),getIP6Packet)
import           Hans.Lens(view)
import           Hans.Monad(Hans,io,decode',dropPacket)
import           Hans.Network.Types
import           Hans.Tcp.Input(processTcp)
import           Hans.Types(NetworkStack)
import           Hans.Udp.Input(processUdp)

-- |Process a packet that has arrived from a device.
processIP6 :: NetworkStack -> Device -> S.ByteString -> Hans ()
processIP6 ns dev payload =
  do ((hdr,hdrLen,bodyLen),body) <- decode' (devStats dev) getIP6Packet payload
     let packetValid = coIP6 (view rxOffload dev)
                    || 0 == computeChecksum (S.take hdrLen payload)
     unless packetValid (dropPacket (devStats dev))
     -- XXX: another place to do IP routing if we implemented HaNS as a router
     checkDestination ns dev (ip6DestAddr hdr)
     handleIP6 ns dev (Just (hdrLen,payload)) hdr (S.take bodyLen body)

-- |The processing stage after the packet has been decoded and validated. It's
-- exposed here (just like the IP4 version) so that routing to an address that's
-- managed by the network stack can skip the device layer.
handleIP6 :: NetworkStack -> Device -> Maybe (Int, S.ByteString) ->
             IP6Header -> S.ByteString ->
             Hans ()
handleIP6 ns dev mbOrig hdr@IP6Header{..} body =
  case ip6NextHeader of
    PROT_UDP            ->
      do routed <- processUdp ns dev ip6SourceAddr ip6DestAddr body
         case (routed, mbOrig) of
           (False, Just (ihl,orig)) -> io
              $ portUnreachable ns dev ip6DestAddr ip6SourceAddr
              $ S.take (ihl + 8) orig
           _ -> return ()

    PROT_TCP            -> processTcp ns dev ip6SourceAddr ip6DestAddr body

    PROT_IP6_HOP_BY_HOP -> undefined
    PROT_IP6_ROUTING    -> undefined
    PROT_IP6_FRAGMENT   -> undefined
    PROT_IP6_ICMP       -> undefined
    PROT_IP6_NO_NEXT    -> return () -- not dropped, just done
    PROT_IP6_DEST_OPTS  -> undefined
    _                   -> dropPacket (devStats dev)

-- |See if the destination of the packet is for us, or drop the packet if not.
checkDestination :: NetworkStack -> Device -> IP6 -> Hans ()
checkDestination = undefined

portUnreachable = undefined
