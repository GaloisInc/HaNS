{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Hans.IP6.Input(
         processIP6
       , handleIP6
       )
 where

import           Control.Monad(unless, forM_)
import           Data.Array ((!),(//))
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Hans.Addr
import           Hans.Checksum(computeChecksum)
import           Hans.Device(Device(..),ChecksumOffload(..),rxOffload)
import           Hans.IP6.Packet(IP6Header(..),getIP6Packet,
                                 IP6OptionsHeader(..),getIP6OptionsHeader,
                                 IP6OptionsHeaderKind(..),IP6Option(..),
                                 IP6Routing(..),getIP6Routing,rhAddresses,
                                 IP6Fragment(..),getIP6Fragment)
import           Hans.IP6.State(ip6Fragments, ip6State)
import           Hans.Lens(view)
import           Hans.Monad(Hans,io,decode',dropPacket)
import           Hans.Network.Fragments(processFragment)
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
     -- XXX: another place to do IP routing if we implemented HaNS as a router.
     -- Note that we'd need to check for and process hop-by-hop and routing options
     -- here, as well
     checkDestination ns dev (ip6DestAddr hdr)
     handleIP6 ns dev (Just (hdrLen,payload)) hdr (S.take bodyLen body)

-- |The processing stage after the packet has been decoded and validated. It's
-- exposed here (just like the IP4 version) so that routing to an address that's
-- managed by the network stack can skip the device layer.
handleIP6 :: NetworkStack -> Device -> Maybe (Int, S.ByteString) ->
             IP6Header -> S.ByteString ->
             Hans ()
handleIP6 ns dev mbOrig hdr@IP6Header{..} origBody =
  process ip6NextHeader origBody
 where
  process PROT_UDP body =
    do routed <- processUdp ns dev ip6SourceAddr ip6DestAddr body
       case (routed, mbOrig) of
         (False, Just (ihl,orig)) -> io
            $ portUnreachable ns dev ip6DestAddr ip6SourceAddr
            $ S.take (ihl + 8) orig
         _ -> return ()

  process PROT_TCP body =
    processTcp ns dev ip6SourceAddr ip6DestAddr body

  process prot body | (prot == PROT_IP6_HOP_BY_HOP)
                   || (prot == PROT_IP6_DEST_OPTS) =
    do let prot' | prot == PROT_IP6_HOP_BY_HOP = HopByHop
                 | otherwise                   = Destination
       (oh,body') <- decode' (devStats dev) (getIP6OptionsHeader prot') body
       forM_ (ohOptions oh) $ \ opt ->
         case opt of
           Pad1                      -> return ()
           PadN                  _   -> return ()
           UnknownSkip           _ _ -> return ()
           UnknownDiscard        _ _ -> dropPacket (devStats dev)
           _                         ->
             parameterProblem ns dev ip6DestAddr ip6SourceAddr 2 (findOption opt body)
       process (ohNextHeader oh) body'

  process PROT_IP6_ROUTING body =
    do (route, body') <- decode' (devStats dev) getIP6Routing body
       let segsLeft' = rhSegmentsLeft route - 1
           n         = rhExtensionLength route `div` 2
           i         = n - rhSegmentsLeft route
       if | rhSegmentsLeft route == 0 ->
              do let hdr' = hdr{ ip6NextHeader = rhNextHeader route }
                 handleIP6 ns dev mbOrig hdr' body'
          | odd (rhExtensionLength route) ->
              parameterProblem ns dev ip6DestAddr ip6SourceAddr 0 body
          | view multicast ip6DestAddr ->
              dropPacket (devStats dev)
          | Just addrs <- rhAddresses route, view multicast (addrs ! i) ->
              dropPacket (devStats dev)
          | ip6HopLimit <= 1 ->
              hopLimitExceeded ns dev ip6DestAddr ip6SourceAddr
          | Just addrs <- rhAddresses route ->
              do let addrs' = addrs // [(i,ip6DestAddr)]
                     hdr'   = hdr{ ip6DestAddr = addrs ! i }
                 undefined addrs' hdr' -- FIXME
          | otherwise ->
              dropPacket (devStats dev)

  process PROT_IP6_FRAGMENT body =
    do (frag, body') <- decode' (devStats dev) (getIP6Fragment hdr) body
       let fragTable = ip6Fragments (view ip6State ns)
       (frag',body'') <- processFragment fragTable frag body'
       process (frNextHeader frag') body'' 

  process PROT_IP6_ICMP body =
    undefined

  process PROT_IP6_NO_NEXT body =
    return () -- not dropped, just done


-- |See if the destination of the packet is for us, or drop the packet if not.
checkDestination :: NetworkStack -> Device -> IP6 -> Hans ()
checkDestination = undefined

hopLimitExceeded = undefined
portUnreachable = undefined
findOption = undefined

parameterProblem = undefined
