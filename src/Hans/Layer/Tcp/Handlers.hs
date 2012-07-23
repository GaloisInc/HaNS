module Hans.Layer.Tcp.Handlers (
    handleIncomingTcp
  ) where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Message.Tcp
import qualified Hans.Layer.IP4 as IP4

import Control.Monad (mzero,mplus,guard)
import Data.Serialize (runGet)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4 -> IP4 -> S.ByteString -> Tcp ()
handleIncomingTcp src dst bytes = do
  (hdr,body) <- liftRight (runGet getTcpPacket bytes)
  established src dst hdr body
    `mplus` initializing src dst hdr
    `mplus` sendSegment src (mkRstAck hdr) L.empty

-- | Handle a message for an already established connection.
established :: IP4 -> IP4 -> TcpHeader -> S.ByteString -> Tcp ()
established remote _local hdr _body = do
  let ident = incomingSocketId remote hdr
  con <- getConnection ident
  case tcpState con of

    -- connection has been accepted, yield out via the connections continuation
    SynReceived | isAck hdr -> do
      setConnection ident con { tcpState = Established }

    Established -> do
      output (putStrLn "got a message for an established connection")

    _ -> do
      output (print (tcpState con))
      mzero

-- | Different states for connections that are being established.
initializing :: IP4 -> IP4 -> TcpHeader -> Tcp ()
initializing remote local hdr
  | isSyn hdr = listening remote local hdr
  | isAck hdr = startsConnnection remote local hdr
  | otherwise = mzero

-- | Handle an attempt to create a connection on a listening port.
listening :: IP4 -> IP4 -> TcpHeader -> Tcp ()
listening remote _local hdr = do
  lcon <- getListeningConnection (listenSocketId (tcpDestPort hdr))
  let child = incomingSocketId remote hdr
  newConnection child SynSent
  synAck remote hdr

-- | Handle a connection finalization.
startsConnnection :: IP4 -> IP4 -> TcpHeader -> Tcp ()
startsConnnection remote local hdr = do
  let child = incomingSocketId remote hdr
  -- XXX if this fails, the socket needs to be closed and gc'd
  con <- getConnection child
  k   <- getAcceptor (listenSocketId (tcpDestPort hdr))
  setConnection child con { tcpState = Established }
  output (k child)


-- Outgoing Packets ------------------------------------------------------------

-- | Send out a tcp segment via the IP layer.
sendSegment :: IP4 -> TcpHeader -> L.ByteString -> Tcp ()
sendSegment dst hdr body = do
  ip4 <- ip4Handle
  output $ IP4.withIP4Source ip4 dst $ \ src ->
    let pkt = renderWithTcpChecksumIP4 src dst hdr body
     in IP4.sendIP4Packet ip4 tcpProtocol dst pkt

-- | Respond to a SYN message with a SYN ACK message.
synAck :: IP4 -> TcpHeader -> Tcp ()
synAck remote hdr = sendSegment remote (mkSynAck (TcpSeqNum 0) hdr) L.empty


-- Guards ----------------------------------------------------------------------

getListeningConnection :: SocketId -> Tcp TcpSocket
getListeningConnection sid = do
  tcp <- getConnection sid
  guard (tcpState tcp == Listen && isAccepting tcp)
  return tcp

getAcceptor :: SocketId -> Tcp Acceptor
getAcceptor sid = do
  tcp <- getConnection sid
  guard (tcpState tcp == Listen)
  case popAcceptor tcp of
    Just (k,tcp') -> do
      setConnection sid tcp'
      return k
    Nothing -> mzero
