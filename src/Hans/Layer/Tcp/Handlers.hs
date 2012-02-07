module Hans.Layer.Tcp.Handlers (
    handleIncomingTcp
  , handleOutgoing
  ) where

import Hans.Address.IP4 (IP4,convertFromWord32)
--import Hans.Channel (send)
import Hans.Layer (output,liftRight)
import Hans.Layer.IP4 (sendIP4Packet,withIP4Source)
import Hans.Layer.Tcp.Monad
    (Tcp,TcpState(..),ip4Handle,ip4Handle,ip4Handle,ip4Handle)
--import Hans.Layer.Timer (udelay)
import Hans.Message.Tcp
    (tcpProtocol,renderWithTcpChecksumIP4,getTcpPacket,recreateTcpChecksumIP4
    ,TcpHeader(..))

import Network.TCP.LTS.In (tcp_deliver_in_packet)
import Network.TCP.Type.Base (get_ip,IPAddr(..))
import Network.TCP.Type.Datagram
    (ICMPDatagram(..),UDPDatagram(..),TCPSegment(..),IPMessage(..)
    ,mkTCPSegment)
import Network.TCP.Type.Socket (Host(..))

import Control.Monad (guard)
import Data.Serialize (runGet)
import MonadLib (get,set)
import qualified Data.ByteString as S


-- | Handle a TCP message from the IP4 layer.
handleIncomingTcp :: IP4 -> IP4 -> S.ByteString -> Tcp ()
handleIncomingTcp src dst bytes = do
  let cs = recreateTcpChecksumIP4 src dst bytes
  (hdr,body) <- liftRight (runGet getTcpPacket bytes)
  guard (tcpChecksum hdr == cs)
  tcp_deliver_in_packet (mkTCPSegment src dst hdr body)

-- | Force packets out of the pure layer.
handleOutgoing :: Tcp ()
handleOutgoing  = do
  s <- get
  let h = tcpHost s
  set (s { tcpHost = h { output_queue = [], ready_list = [] } })
  let msgs = output_queue h
  mapM_ deliverIPMessage msgs
  let ready = ready_list h
  mapM_ output ready

deliverIPMessage :: IPMessage -> Tcp ()
deliverIPMessage msg =
  case msg of
    TCPMessage  seg  -> deliverTCPSegment   seg
    ICMPMessage icmp -> deliverICMPDatagram icmp
    UDPMessage  udp  -> deliverUDPDatagram  udp

deliverTCPSegment :: TCPSegment -> Tcp ()
deliverTCPSegment seg = do
  let hdr        = tcp_header seg
      IPAddr dst = get_ip (tcp_dst seg)
      dstAddr    = convertFromWord32 dst
  ip4 <- ip4Handle
  output $ withIP4Source ip4 dstAddr $ \ srcAddr -> do
    let pkt = renderWithTcpChecksumIP4 srcAddr dstAddr hdr (tcp_data seg)
    sendIP4Packet ip4 tcpProtocol dstAddr pkt

deliverICMPDatagram :: ICMPDatagram -> Tcp ()
deliverICMPDatagram _icmp = do
  output (putStrLn "Ignoring TCP icmp packet")

deliverUDPDatagram :: UDPDatagram -> Tcp ()
deliverUDPDatagram _udp = do
  output (putStrLn "Ignoring TCP udp packet")

