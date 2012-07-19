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
    `mplus` connecting src dst hdr body
    `mplus` sendSegment src (mkRstAck hdr) L.empty

-- | Handle a message for an already established connection.
established :: IP4 -> IP4 -> TcpHeader -> S.ByteString -> Tcp ()
established remote local hdr _body = do
  con <- getConnection remote local hdr
  output (print con)

-- | Handle an attempt to create a connection on a listening port.
connecting :: IP4 -> IP4 -> TcpHeader -> S.ByteString -> Tcp ()
connecting remote local hdr _body = do
  requireListening local (tcpDestPort hdr)
  output (putStrLn ("connection from: " ++ show (remote,local,hdr)))


-- Outgoing Packets ------------------------------------------------------------

-- | Send out a tcp segment via the IP layer.
sendSegment :: IP4 -> TcpHeader -> L.ByteString -> Tcp ()
sendSegment dst hdr body = do
  ip4 <- ip4Handle
  output $ IP4.withIP4Source ip4 dst $ \ src ->
    let pkt = renderWithTcpChecksumIP4 src dst hdr body
     in IP4.sendIP4Packet ip4 tcpProtocol dst pkt


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
