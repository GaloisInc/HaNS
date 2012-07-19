module Hans.Layer.Tcp.Handlers (
    handleIncomingTcp
  ) where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Connection
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
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
    `mplus` startConnection src dst hdr
    `mplus` connected src dst hdr
    `mplus` sendSegment src (mkRstAck hdr) L.empty

-- | Handle a message for an already established connection.
established :: IP4 -> IP4 -> TcpHeader -> S.ByteString -> Tcp ()
established remote local hdr _body = do
  con <- getConnection remote local hdr
  output (print con)

-- | Handle an attempt to create a connection on a listening port.
startConnection :: IP4 -> IP4 -> TcpHeader -> Tcp ()
startConnection remote local hdr = do
  guard (isSyn hdr)
  requireListening local (tcpDestPort hdr)
  output (putStrLn "startConnection")
  synAck remote hdr

-- | Record the fact that the connection has been established.
connected :: IP4 -> IP4 -> TcpHeader -> Tcp ()
connected remote local hdr = do
  guard (isAck hdr)
  newConnection remote local hdr Established
  output (putStrLn "connected")


-- Outgoing Packets ------------------------------------------------------------

-- | Send out a tcp segment via the IP layer.
sendSegment :: IP4 -> TcpHeader -> L.ByteString -> Tcp ()
sendSegment dst hdr body = do
  ip4 <- ip4Handle
  output $ IP4.withIP4Source ip4 dst $ \ src ->
    let pkt = renderWithTcpChecksumIP4 src dst hdr body
     in IP4.sendIP4Packet ip4 tcpProtocol dst pkt

synAck :: IP4 -> TcpHeader -> Tcp ()
synAck remote hdr = sendSegment remote (mkSynAck (TcpSeqNum 0) hdr) L.empty


-- Guards ----------------------------------------------------------------------

-- | Require that a listening connection exists.
requireListening :: IP4 -> TcpPort -> Tcp ()
requireListening local port = do
  cons <- getListenConnections
  let lcon = ListenConnection { lcHost = local, lcPort = port }
  guard (isListening lcon cons)

-- | Lookup a connection in the internal connection map.  If the connection does
-- not exist, emit a RST ACK and fail the rest of the computation with mzero.
getConnection :: IP4 -> IP4 -> TcpHeader -> Tcp Connection
getConnection remote local hdr = do
  cons <- getConnections
  case lookupConnection (incomingConnIdent remote local hdr) cons of
    Just con -> return con
    Nothing  -> do
      mzero

-- | Create a new connection.
newConnection :: IP4 -> IP4 -> TcpHeader -> ConnectionState -> Tcp ()
newConnection remote local hdr state = do
  cons <- getConnections
  let ident = incomingConnIdent remote local hdr
      con   = emptyConnection state
  setConnections (addConnection ident con cons)
