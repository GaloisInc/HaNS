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
    `mplus` listening src dst hdr
    `mplus` sendSegment src (mkRstAck hdr) L.empty

-- | Handle a message for an already established connection.
established :: IP4 -> IP4 -> TcpHeader -> S.ByteString -> Tcp ()
established remote local hdr _body = do
  let ident = incomingConnIdent remote local hdr
  con <- getConnection ident
  case conState con of

    SynRcvd | isAck hdr -> setConnection ident con { conState = Established }

    _ -> mzero

-- | Handle an attempt to create a connection on a listening port.
listening :: IP4 -> IP4 -> TcpHeader -> Tcp ()
listening remote local hdr = do
  guard (isSyn hdr)
  _con <- getListening local (tcpDestPort hdr)
  let ident = incomingConnIdent remote local hdr
  newConnection ident SynRcvd
  synAck remote hdr


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

-- | Require that a listening connection exists.
getListening :: IP4 -> TcpPort -> Tcp ListenConnection
getListening local port = do
  cons <- getListenConnections
  case lookupListeningConnection local port cons of
    Just con -> return con
    Nothing  -> mzero

-- | Lookup a connection in the internal connection map.  If the connection does
-- not exist, emit a RST ACK and fail the rest of the computation with mzero.
getConnection :: ConnIdent -> Tcp Connection
getConnection ident = do
  cons <- getConnections
  case lookupConnection ident cons of
    Just con -> return con
    Nothing  -> mzero

setConnection :: ConnIdent -> Connection -> Tcp ()
setConnection ident con = do
  cons <- getConnections
  setConnections (addConnection ident con cons)

-- | Create a new connection.
newConnection :: ConnIdent -> ConnectionState -> Tcp ()
newConnection ident state = setConnection ident (emptyConnection state)
