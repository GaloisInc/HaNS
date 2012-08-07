module Hans.Layer.Tcp.Handlers where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Timers
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp
import Hans.Utils

import Control.Monad (mzero,mplus,guard)
import Data.Maybe (fromMaybe)
import Data.Serialize (runGet)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4 -> IP4 -> S.ByteString -> Tcp ()
handleIncomingTcp src dst bytes = do
  (hdr,body) <- liftRight (runGet getTcpPacket bytes)
  let cs = recreateTcpChecksumIP4 src dst bytes
  guard (cs == tcpChecksum hdr)
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
        | isAck hdr -> handleAck hdr
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
        | isAck hdr -> closeSocket

      -- avoid sending things to a closed socket; this socket might just be
      -- waiting for a user signal to be gc'd
      Closed -> mzero

      _ -> outputS (putStrLn ("Unexpected packet for state " ++ show state))


deliverSegment :: TcpHeader -> S.ByteString -> Sock ()
deliverSegment _hdr body = do
  mb <- modifyTcpSocket $ \ tcp -> fromMaybe (Nothing,tcp) $ do
    (wakeup,bufIn) <- putBytes (chunk body) (tcpInBuffer tcp)
    let tcp' = tcp
          { tcpRcvNxt      = tcpRcvNxt tcp + fromIntegral (S.length body)
          , tcpInBuffer    = bufIn
          , tcpNeedsDelAck = True
          }
    return (wakeup, tcp')

  case mb of
    Just wakeup -> outputS (tryAgain wakeup)
    Nothing     -> return ()

-- | Handle an ACK to a sent data segment.
handleAck :: TcpHeader -> Sock ()
handleAck hdr = do
  modifyTcpSocket_ updateAck
  outputSegments
  where
  updateAck tcp = case receiveAck hdr (tcpOut tcp) of
    Just out' -> tcp { tcpOut = out' }
    Nothing   -> tcp

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
    let childSock = (emptyTcpSocket (tcpWindow hdr))
          { tcpParent   = Just parent
          , tcpSocketId = incomingSocketId remote hdr
          , tcpState    = SynSent
          , tcpSndNxt   = isn
          , tcpSndUna   = isn
          , tcpRcvNxt   = tcpSeqNum hdr
          }
    withChild childSock synAck


-- Buffer Delivery -------------------------------------------------------------

-- | Fill up the remote window with segments.
outputSegments :: Sock ()
outputSegments  = do
  (mb,segs) <- modifyTcpSocket genSegments
  F.mapM_ outputSegment segs
  case mb of
    Nothing     -> return ()
    Just wakeup -> outputS (tryAgain wakeup)

-- | Take data from the output buffer, and turn it into segments.
genSegments :: TcpSocket -> ((Maybe Wakeup,Seq.Seq Segment),TcpSocket)
genSegments tcp0 = loop Nothing Seq.empty tcp0
  where
  loop mb segs tcp
    | winAvailable (tcpOut tcp) <= 0 = done
    | otherwise                      = fromMaybe done $ do
      let len = nextSegSize tcp
      (mbWakeup,body,bufOut) <- takeBytes len (tcpOutBuffer tcp)
      let seg  = Segment
            { segAckNum = tcpSndNxt tcp + fromIntegral (L.length body)
            , segHeader = mkData tcp
            , segBody   = body
            }
          tcp' = tcp
            { tcpSndNxt    = segAckNum seg
            , tcpOut       = addSegment seg (tcpOut tcp)
            , tcpOutBuffer = bufOut
            }

      return (loop (mb `mplus` mbWakeup) (segs Seq.|> seg) tcp')
    where
    done = ((mb,segs),tcp)
