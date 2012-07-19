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

import Control.Monad (mzero)
import Data.Serialize (runGet)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4 -> IP4 -> S.ByteString -> Tcp ()
handleIncomingTcp src dst bytes = do
  (hdr,_body) <- liftRight (runGet getTcpPacket bytes)
  con <- getIncomingConnection src dst hdr
  output (print con)

-- | Lookup a connection in the internal connection map.  If the connection does
-- not exist, emit a RST ACK and fail the rest of the computation with mzero.
getIncomingConnection :: IP4 -> IP4 -> TcpHeader -> Tcp Connection
getIncomingConnection remote local hdr = do
  cons <- getConnections
  case lookupConnection (incomingConnIdent remote local hdr) cons of
    Just con -> return con
    Nothing  -> do
      sendSegment remote (mkRstAck hdr) L.empty
      mzero


-- Outgoing Packets ------------------------------------------------------------

sendSegment :: IP4 -> TcpHeader -> L.ByteString -> Tcp ()
sendSegment dst hdr body = do
  ip4 <- ip4Handle
  output $ IP4.withIP4Source ip4 dst $ \ src ->
    let pkt = renderWithTcpChecksumIP4 src dst hdr body
     in IP4.sendIP4Packet ip4 tcpProtocol dst pkt
