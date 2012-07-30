module Hans.Layer.Tcp.Handlers where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Message.Tcp

import Control.Monad (mzero,mplus)
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
established remote _local hdr body = do
  let sid = incomingSocketId remote hdr
  establishedConnection sid $ do
    state <- getState
    case state of

      Established
        | isFinAck hdr -> do
          advanceRcvNxt 1
          ack
          -- technically, we go to CloseWait now, but we'll transition out as
          -- soon as we go to LastAck
          finAck
          setState LastAck
        | otherwise -> deliverSegment hdr body

      SynSent
        | isAck hdr -> do
          setState Established
          k <- inParent popAcceptor
          outputS (k sid)

      FinWait1
          -- 3-way close
        | isFinAck hdr -> do
          advanceRcvNxt 1
          ack
          closeSocket
          -- 4-way close
        | isAck hdr -> do
          advanceRcvNxt 1
          setState FinWait2

      FinWait2
        | isFinAck hdr -> do
          ack
          closeSocket

      LastAck
        | isAck hdr -> do
          closeSocket

      _ -> outputS (putStrLn ("Unexpected packet for state " ++ show state))


deliverSegment :: TcpHeader -> S.ByteString -> Sock ()
deliverSegment _hdr body = do
  advanceRcvNxt (fromIntegral (S.length body))
  outputS (putStrLn "deliverSegment")
  delayedAck

-- XXX schedule a delay to put the socket into a closed state.
closeSocket :: Sock ()
closeSocket  = do
  setState TimeWait
  runClosed

-- | Different states for connections that are being established.
initializing :: IP4 -> IP4 -> TcpHeader -> Tcp ()
initializing remote local hdr
  | isSyn hdr = listening remote local hdr
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
          , tcpSndNxt   = 0
          , tcpSndUna   = 0
          , tcpRcvNxt   = tcpSeqNum hdr
          , tcpSockWin  = tcpWindow hdr
          }
    withChild childSock (synAck remote)
