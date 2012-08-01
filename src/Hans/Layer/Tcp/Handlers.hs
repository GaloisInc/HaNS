module Hans.Layer.Tcp.Handlers where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Timers
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
          enterTimeWait
          -- 4-way close
        | isAck hdr -> do
          advanceRcvNxt 1
          setState FinWait2

      FinWait2
        | isFinAck hdr -> do
          ack
          enterTimeWait

      LastAck
        | isAck hdr -> setState Closed

      -- avoid sending things to a closed socket; this socket might just be
      -- waiting for a user signal to be gc'd
      Closed -> mzero

      _ -> outputS (putStrLn ("Unexpected packet for state " ++ show state))


deliverSegment :: TcpHeader -> S.ByteString -> Sock ()
deliverSegment _hdr body = do
  advanceRcvNxt (fromIntegral (S.length body))
  outputS (putStrLn "deliverSegment")
  delayedAck

enterTimeWait :: Sock ()
enterTimeWait  = do
  set2MSL mslTimeout
  setState TimeWait

-- | Different states for connections that are being established.
initializing :: IP4 -> IP4 -> TcpHeader -> Tcp ()
initializing remote local hdr
  | isSyn hdr = listening remote local hdr
  | otherwise = mzero

-- | Handle an attempt to create a connection on a listening port.
listening :: IP4 -> IP4 -> TcpHeader -> Tcp ()
listening remote _local hdr = do
  let parent = listenSocketId (tcpDestPort hdr)
  isn <- initialSeqNum
  listeningConnection parent $ do
    let childSock = emptyTcpSocket
          { tcpParent   = Just parent
          , tcpSocketId = incomingSocketId remote hdr
          , tcpState    = SynSent
          , tcpSndNxt   = isn
          , tcpSndUna   = isn
          , tcpRcvNxt   = tcpSeqNum hdr
          , tcpSockWin  = tcpWindow hdr
          }
    withChild childSock (synAck remote)
