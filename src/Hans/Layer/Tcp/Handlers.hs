module Hans.Layer.Tcp.Handlers (
    handleIncomingTcp
  , sendSegment
  ) where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Message.Tcp

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
established remote _local hdr _body =
  establishedConnection (incomingSocketId remote hdr) $ do
    state <- getState
    case state of

      -- common case, sending data.
      Established
        | isFin hdr -> do
          outputS $ putStrLn "closing the socket!"
        | otherwise -> do
          outputS $ putStrLn "got a message for an established connection"

      _ -> do
        inTcp $ output $ print state
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
  let parent = listenSocketId (tcpDestPort hdr)
  listeningConnection parent $ do
    let child     = incomingSocketId remote hdr
        childSock = emptyTcpSocket
          { tcpState    = SynSent
          , tcpParent   = Just parent
          -- XXX this should really be changed
          , tcpSockSeq  = 0
          , tcpSockAck  = tcpSeqNum hdr
          , tcpSocketId = child
          }
    addChildConnection child childSock
    synAck childSock remote

-- | Handle a connection finalization.
startsConnnection :: IP4 -> IP4 -> TcpHeader -> Tcp ()
startsConnnection remote local hdr = do
  let child = incomingSocketId remote hdr
  -- XXX if this fails, the socket needs to be closed and gc'd
  establishedConnection child $ do
    k <- getAcceptor (listenSocketId (tcpDestPort hdr))
    setState Established
    outputS (k child)


-- Outgoing Packets ------------------------------------------------------------

-- | Respond to a SYN message with a SYN ACK message.
synAck :: TcpSocket -> IP4 -> Sock ()
synAck tcp remote = inTcp (sendSegment remote (mkSynAck tcp) L.empty)


-- Guards ----------------------------------------------------------------------

getAcceptor :: SocketId -> Sock Acceptor
getAcceptor sid = inTcp $ do
  tcp <- getConnection sid
  guard (tcpState tcp == Listen)
  case popAcceptor tcp of
    Just (k,tcp') -> do
      setConnection sid tcp'
      return k
    Nothing -> mzero
