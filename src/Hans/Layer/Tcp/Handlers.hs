module Hans.Layer.Tcp.Handlers where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Timers
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Ip4
import Hans.Message.Tcp

import Control.Monad (mzero,mplus,guard,when)
import Data.Bits (bit)
import Data.Int (Int64)
import Data.Maybe (fromMaybe,isJust,isNothing)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4Header -> S.ByteString -> Tcp ()
handleIncomingTcp ip4 bytes = do
  let src = ip4SourceAddr ip4
      dst = ip4DestAddr ip4
  guard (validateTcpChecksumIP4 src dst bytes)
  (hdr,body) <- liftRight (parseTcpPacket bytes)
  established src dst hdr body
    `mplus` listening ip4 hdr
    `mplus` sendSegment src (mkRstAck hdr) L.empty


-- Established Connections -----------------------------------------------------

-- | Handle a message for an already established connection.
established :: IP4 -> IP4 -> TcpHeader -> S.ByteString -> Tcp ()
established remote _local hdr body = do
  let sid = incomingSocketId remote hdr
  establishedConnection sid $ do
    state <- getState
    resetIdle

    -- keep their timestamp up to date
    shouldDrop <- modifyTcpSocket (updateTimestamp hdr)
    guard (not shouldDrop)

    case state of

      Established
        | isFinAck hdr -> remoteGracefulTeardown
        | tcpRst hdr   -> closeSocket
        | otherwise    -> deliverSegment hdr body

      SynReceived
        | isAck hdr -> do
          setState Established
          k <- inParent popAcceptor
          outputS (k sid)
          -- close this child socket
        | tcpRst hdr -> closeSocket
          -- retransmitted syn
        | isSyn hdr -> synAck

          -- non-synchronized state
        | otherwise -> do
          rst
          closeSocket

      SynSent
          -- connection rejected
        | isRstAck hdr -> do
          setState Closed
          notify False
          closeSocket
          -- connection ack'd
        | isSynAck hdr -> do
          modifyTcpSocket_ $ \ tcp -> tcp
            { tcpState       = Established
            , tcpOutMSS      = fromMaybe (tcpInMSS tcp) (getMSS hdr)
            , tcpOut         = setSndWind (tcpWindow hdr)
                             $ setSndWindScale (windowScale hdr)
                             $ tcpOut tcp
            , tcpIn          = emptyLocalWindow (tcpSeqNum hdr) 14600 0
            , tcpSack        = sackSupported hdr
            , tcpWindowScale = isJust (findTcpOption OptTagWindowScaling hdr)
            }
          advanceRcvNxt 1
          ack

          notify True

      FinWait1
          -- simultaneous close
        | isFin hdr -> do
          advanceRcvNxt 1
          ack
          setState Closing

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
        | isFin hdr || isFinAck hdr -> do
          ack
          enterTimeWait

        | otherwise ->
          return ()

      Closing
        | isAck hdr -> do
          advanceRcvNxt 1
          enterTimeWait

      LastAck
        | isAck hdr -> closeSocket

      -- avoid sending things to a closed socket; this socket might just be
      -- waiting for a user signal to be gc'd
      Closed -> mzero

      _ -> outputS (putStrLn ("Unexpected packet for state " ++ show state))


-- | Update the currently held timestamp for both sides, and return a boolean
-- that indicates whether or not the packet should be dropped.
--
-- RFC 1323
updateTimestamp :: TcpHeader -> TcpSocket -> (Bool,TcpSocket)
updateTimestamp hdr tcp = (shouldDrop,tcp { tcpTimestamp = ts' })
  where
  -- when the timestamp check fails from an ack, that's not a syn,ack, mark this
  -- packet as one to be dropped.
  shouldDrop = not (tcpSyn hdr || tcpRst hdr)
            && isJust (tcpTimestamp tcp)
            && isNothing ts'
  ts' = do
    ts                     <- tcpTimestamp tcp
    OptTimestamp them echo <- findTcpOption OptTagTimestamp hdr
    -- when this is an ack, the echo value should not be greater than the
    -- current timestamp.  this doesn't currently account for overflow, so
    -- connections lasting >27 days will probably fail.
    let rel       = tsTimestamp ts - echo
        isGreater = 0 < rel && rel < bit 31
    when (tcpAck hdr) (guard (tsTimestamp ts == echo || isGreater))
    return ts { tsLastTimestamp = them }

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
    mb <- modifyTcpSocket (handleData hdr body)
    case mb of
      Just wakeup -> outputS (tryAgain wakeup)
      Nothing     -> return ()

  -- handle graceful teardown, initiated remotely
  when (tcpFin hdr) remoteGracefulTeardown

-- | Enqueue a new packet in the local window, attempting to place the bytes
-- received in the user buffer as they complete a part of the stream.  Bytes are
-- ack'd as they complete the stream, and bytes that would cause the local
-- buffer to overflow are dropped.
handleData :: TcpHeader -> S.ByteString -> TcpSocket
           -> (Maybe Wakeup, TcpSocket)
handleData hdr body tcp0 = fromMaybe (Nothing,tcp) $ do
  (wakeup,buf') <- putBytes bytes (tcpInBuffer tcp)
  let tcp' = tcp
        { tcpInBuffer = buf'
        , tcpTimers   = (tcpTimers tcp)
          { ttDelayedAck = not (L.null bytes)
          }
        }
  return (wakeup, tcp')
  where
  (segs,win') = incomingPacket hdr body (tcpIn tcp0)
  tcp         = tcp0 { tcpIn = win' }
  bytes       = L.fromChunks (map inBody (F.toList segs))


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
      let calibrate | outFresh seg = calibrateRTO now (outTime seg)
                    | otherwise    = id
       in tcp { tcpOut    = out'
              , tcpSndUna = tcpAckNum hdr
              , tcpTimers = calibrate (tcpTimers tcp)
              }
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

-- | Setup the 2MSL timer, and enter the TIME_WAIT state.
enterTimeWait :: Sock ()
enterTimeWait  = do
  set2MSL mslTimeout
  setState TimeWait


-- Listening Connections -------------------------------------------------------

-- | Handle an attempt to create a connection on a listening port.
listening :: IP4Header -> TcpHeader -> Tcp ()
listening ip4 hdr = do
  guard (isSyn hdr)
  let parent = listenSocketId (tcpDestPort hdr)
  isn <- initialSeqNum
  listeningConnection parent $ do
    tcp <- getTcpSocket
    let childSock = (emptyTcpSocket (tcpWindow hdr) (windowScale hdr))
          { tcpParent      = Just parent
          , tcpSocketId    = incomingSocketId (ip4SourceAddr ip4) hdr
          , tcpState       = SynReceived
          , tcpSndNxt      = isn
          , tcpSndUna      = isn
          , tcpIn          = emptyLocalWindow (tcpSeqNum hdr) 14600 0
          , tcpOutMSS      = fromMaybe defaultMSS (getMSS hdr)
          , tcpTimestamp   = do
              -- require that the parent had a timestamp
              ts <- tcpTimestamp tcp
              -- require that they have sent us a timestamp, before using them
              OptTimestamp val _ <- findTcpOption OptTagTimestamp hdr
              return ts { tsLastTimestamp = val }
          , tcpSack        = sackSupported hdr
          , tcpWindowScale = isJust (findTcpOption OptTagWindowScaling hdr)
          }
    withChild childSock synAck


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
genSegments :: POSIXTime -> TcpSocket -> ((Maybe Wakeup,OutSegments),TcpSocket)
genSegments now tcp0 = loop Nothing Seq.empty tcp0
  where
  loop mb segs tcp
    | rwAvailable (tcpOut tcp) <= 0 = done
    | otherwise                     = fromMaybe done $ do
      let len = nextSegSize tcp
      (mbWakeup,body,bufOut) <- takeBytes len (tcpOutBuffer tcp)
      let seg  = OutSegment
            { outAckNum = tcpSndNxt tcp + fromIntegral (L.length body)
            , outTime   = now
            , outFresh  = True
            , outHeader = mkData tcp
            , outRTO    = ttRTO (tcpTimers tcp)
            , outBody   = body
            }
          tcp' = tcp
            { tcpSndNxt    = outAckNum seg
            , tcpOut       = addSegment seg (tcpOut tcp)
            , tcpOutBuffer = bufOut
            }

      return (loop (mb `mplus` mbWakeup) (segs Seq.|> seg) tcp')
    where
    done | not (Seq.null segs) = ((mb,segs),tcp)
         | otherwise           = ((mb,segs),tcp)


-- Utilities -------------------------------------------------------------------

-- | Extract the maximum segment size from a header, if the option is present.
getMSS :: TcpHeader -> Maybe Int64
getMSS hdr = do
  OptMaxSegmentSize n <- findTcpOption OptTagMaxSegmentSize hdr
  return (fromIntegral n)

windowScale :: TcpHeader -> Int
windowScale hdr = fromMaybe 0 $ do
  OptWindowScaling n <- findTcpOption OptTagWindowScaling hdr
  return (fromIntegral n)

sackSupported :: TcpHeader -> Bool
sackSupported  = isJust . findTcpOption OptTagSackPermitted
