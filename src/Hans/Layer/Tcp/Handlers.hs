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

import Control.Monad (mzero,mplus,guard,when)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Serialize (runGet)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4 -> IP4 -> S.ByteString -> Tcp ()
handleIncomingTcp src dst bytes = do
  guard (validateTcpChecksumIP4 src dst bytes)
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
        | isFinAck hdr -> remoteGracefulTeardown
        | otherwise    -> deliverSegment hdr body

      SynReceived
        | isAck hdr -> do
          setState Established
          k <- inParent popAcceptor
          outputS (k sid)

      SynSent
          -- connection rejected
        | isRstAck hdr -> do
          setState Closed
          notify False
          closeSocket
          -- connection ack'd
        | isSynAck hdr -> do
          modifyTcpSocket_ $ \ tcp -> tcp
            { tcpState  = Established
            , tcpRcvNxt = tcpSeqNum hdr
            , tcpOutMSS = fromMaybe (tcpInMSS tcp) (getMSS hdr)
            , tcpOut    = resizeWindow (tcpWindow hdr) (tcpOut tcp)
            }
          advanceRcvNxt 1
          ack

          notify True

          tcp <- getTcpSocket
          outputS (print (tcpOutMSS tcp, tcpOut tcp))

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


-- | Deliver incoming bytes to the buffer in a user socket.  If there's no free
-- space in the buffer, the bytes will be dropped.
--
-- XXX should this not ack if it's going to drop packets?
deliverSegment :: TcpHeader -> S.ByteString -> Sock ()
deliverSegment hdr body = do

  -- handle data segment acknowledgments
  when (isAck hdr) (handleAck hdr)

  -- process a message, if there was data attached.  in the case that there is
  -- no room in the buffer, the original socket state will be returned,
  -- preventing any ACK from being sent.
  when (S.length body > 0) $ do
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

  -- handle graceful teardown, initiated remotely
  when (tcpFin hdr) remoteGracefulTeardown

-- | Handle an ACK to a sent data segment.
handleAck :: TcpHeader -> Sock ()
handleAck hdr = do
  now <- inTcp time
  modifyTcpSocket_ (updateAck now)
  outputSegments
  where
  -- using Karns algorithm, only calibrate the RTO if the packet that was ack'd
  -- is fresh.
  updateAck now tcp = case receiveAck hdr (tcpOut tcp) of
    Just (seg,out') ->
      let tcp' = tcp { tcpOut = out', tcpSndUna = tcpAckNum hdr }
       in if segFresh seg
             then calibrateRTO now (segTime seg) tcp'
             else tcp'

    Nothing -> tcp

-- | The other end has sent a FIN packet, acknowledge it, and respond with a
-- FIN,ACK packet.
remoteGracefulTeardown :: Sock ()
remoteGracefulTeardown  = do
  advanceRcvNxt 1
  ack
  -- technically, we go to CloseWait now, but we'll transition out as
  -- soon as we go to LastAck
  finAck
  setState LastAck

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
          , tcpState    = SynReceived
          , tcpSndNxt   = isn
          , tcpSndUna   = isn
          , tcpRcvNxt   = tcpSeqNum hdr
          , tcpOutMSS   = fromMaybe (tcpInMSS childSock) (getMSS hdr)
          }
    withChild childSock synAck

getMSS :: TcpHeader -> Maybe Int64
getMSS hdr = do
  OptMaxSegmentSize n <- findTcpOption OptTagMaxSegmentSize hdr
  return (fromIntegral n)


-- Buffer Delivery -------------------------------------------------------------

-- | Fill up the remote window with segments.
outputSegments :: Sock ()
outputSegments  = do
  now <- inTcp time
  (mb,segs) <- modifyTcpSocket (genSegments now)
  F.mapM_ outputSegment segs
  case mb of
    Nothing     -> return ()
    Just wakeup -> outputS (tryAgain wakeup)

-- | Take data from the output buffer, and turn it into segments.
genSegments :: POSIXTime -> TcpSocket -> ((Maybe Wakeup,Seq.Seq Segment),TcpSocket)
genSegments now tcp0 = loop Nothing Seq.empty tcp0
  where
  loop mb segs tcp
    | winAvailable (tcpOut tcp) <= 0 = done
    | otherwise                      = fromMaybe done $ do
      let len = nextSegSize tcp
      (mbWakeup,body,bufOut) <- takeBytes len (tcpOutBuffer tcp)
      let seg  = Segment
            { segAckNum = tcpSndNxt tcp + fromIntegral (L.length body)
            , segTime   = now
            , segFresh  = True
            , segHeader = mkData tcp
            , segRTO    = tcpRTO tcp
            , segBody   = body
            }
          tcp' = tcp
            { tcpSndNxt    = segAckNum seg
            , tcpOut       = addSegment seg (tcpOut tcp)
            , tcpOutBuffer = bufOut
            }

      return (loop (mb `mplus` mbWakeup) (segs Seq.|> seg) tcp')
    where
    done | not (Seq.null segs) = ((mb,segs),tcp)
         | otherwise           = ((mb,segs),tcp)
