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
established remote _local hdr body =
  establishedConnection (incomingSocketId remote hdr) $ do
    state <- getState
    case state of

      -- common case, sending data.
      Established
        | isFinAck hdr -> outputS (putStrLn "client trying to close")
        | otherwise    -> deliverSegment hdr body

      -- we've sent a fin ack, and are waiting for a response.
      FinWait1
          -- 3-way close
        | isFinAck hdr -> do
          addAckNum 1
          ack
          setState TimeWait
          -- XXX schedule a delay to put the socket into a closed state
          runClosed
          -- 4-way close
        | isAck hdr -> do
          addAckNum 1
          setState FinWait2
        | otherwise -> deliverSegment hdr body

      FinWait2
        | isFinAck hdr -> do
          addSeqNum 1
          setState TimeWait
          ack
          -- XXX schedule a delay to put the socket into a closed state
          runClosed

      _ -> do
        outputS (print state)
        mzero

deliverSegment :: TcpHeader -> S.ByteString -> Sock ()
deliverSegment _hdr body = do
  addAckNum (fromIntegral (S.length body))
  outputS (putStrLn "deliverSegment")
  ack

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
    let childSock = emptyTcpSocket
          { tcpParent   = Just parent
          , tcpSocketId = incomingSocketId remote hdr
          , tcpState    = SynSent
          -- XXX this should really be changed
          , tcpSockSeq  = 0
          , tcpSockAck  = tcpSeqNum hdr
          , tcpSockWin  = tcpWindow hdr
          }
    withChild childSock (synAck remote)

-- | Handle a connection finalization.
startsConnnection :: IP4 -> IP4 -> TcpHeader -> Tcp ()
startsConnnection remote _local hdr = do
  let child = incomingSocketId remote hdr
  -- XXX if this fails, the socket needs to be closed and gc'd
  establishedConnection child $ do
    k <- getAcceptor (listenSocketId (tcpDestPort hdr))
    setState Established
    outputS (k child)


-- Outgoing Packets ------------------------------------------------------------

-- | Respond to a SYN message with a SYN ACK message.
synAck :: IP4 -> Sock ()
synAck remote = do
  addAckNum 1
  tcp <- getTcpSocket
  inTcp (sendSegment remote (mkSynAck tcp) L.empty)
  addSeqNum 1

ack :: Sock ()
ack  = do
  tcp <- getTcpSocket
  inTcp (sendSegment (sidRemoteHost (tcpSocketId tcp)) (mkAck tcp) L.empty)


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
