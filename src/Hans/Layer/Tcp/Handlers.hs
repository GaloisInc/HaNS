{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Monad (guard,when,unless,mplus)
import Data.Bits (bit)
import Data.Int (Int64)
import Data.Maybe (fromMaybe,isJust,isNothing)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

import Debug.Trace


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4Header -> S.ByteString -> Tcp ()
handleIncomingTcp ip4 bytes = do
  let src = ip4SourceAddr ip4
      dst = ip4DestAddr ip4
  guard (validateTcpChecksumIP4 src dst bytes)
  (hdr,body) <- liftRight (parseTcpPacket bytes)

  withConnection src hdr (segmentArrives src hdr body)
    `mplus` noConnection src hdr body

noConnection :: IP4 -> TcpHeader -> S.ByteString -> Tcp ()
noConnection src hdr @ TcpHeader { .. } body =
  do if | tcpRst    -> return ()
        | tcpAck    -> sendSegment src (mkRst hdr)                    L.empty
        | otherwise -> sendSegment src (mkRstAck hdr (S.length body)) L.empty
     finish


-- Segment Arrival -------------------------------------------------------------

{-# INLINE discardAndReturn #-}
discardAndReturn :: Sock a
discardAndReturn  = do outputSegments
                       inTcp finish

{-# INLINE done #-}
done :: Sock a
done  = do outputSegments
           inTcp finish

segmentArrives :: IP4 -> TcpHeader -> S.ByteString -> Sock ()
segmentArrives src hdr body =
  do whenState Closed (inTcp (noConnection src hdr body))

     whenState Listen $
       do when (tcpAck hdr) (rst hdr)

          when (tcpSyn hdr) $ do child <- createConnection src hdr
                                 withChild child synAck

          -- RST will be dropped at this point, as will anything else that
          -- wasn't covered by the above two cases.
          done

     whenState SynSent $
       do tcp <- getTcpSocket

          -- check the ACK
          when (tcpAck hdr) $
            do when (tcpAckNum hdr <= tcpIss tcp ||
                     tcpAckNum hdr >  tcpSndNxt tcp) $
                 do unless (tcpRst hdr) (rst hdr)
                    discardAndReturn

          -- one of these has to be set to continue processing in this state.
          unless (tcpSyn hdr || tcpRst hdr) discardAndReturn

          -- check RST
          let accAcceptable = tcpSndUna tcp <= tcpAckNum hdr &&
                              tcpAckNum hdr <= tcpSndNxt tcp

          when (tcpRst hdr) $
            do when accAcceptable $ do notify False
                                       closeSocket
               discardAndReturn

          -- this is where a security/compartment check would be done

          -- check SYN

          when (tcpSyn hdr) $
            do advanceRcvNxt 1
               modifyTcpSocket_ $ \ tcp -> tcp
                 { tcpOutMSS      = fromMaybe (tcpInMSS tcp) (getMSS hdr)

                   -- clear out, and configure the retransmit buffer
                 , tcpOut         = setSndWind (tcpWindow hdr)
                                  $ setSndWindScale (windowScale hdr)
                                  $ clearRetransmit
                                  $ tcpOut tcp

                   -- this corresponds to setting IRS to SEQ.SEG
                 , tcpIn          = emptyLocalWindow (tcpSeqNum hdr) 14600 0
                 , tcpSack        = sackSupported hdr
                 , tcpWindowScale = isJust (findTcpOption OptTagWindowScaling hdr)
                 }

               when (tcpAck hdr) $
                 do handleAck hdr -- update SND.UNA
                    TcpSocket { .. } <- getTcpSocket
                    if tcpSndUna > tcpIss
                       then do ack
                               establishConnection

                               -- continue at step 6
                               when (tcpUrg hdr) (proceedFromStep6 hdr body)

                       else do setState SynReceived
                               synAck
                               -- XXX queue any additional data for processing
                               -- once Established has been reached

                    done

     -- make sure that the sequence numbers are valid
     checkSequenceNumber hdr body
     checkResetBit hdr body
     -- skip security/precidence check
     checkSynBit hdr body
     deliverSegment hdr body
     checkAckBit hdr body
     proceedFromStep6 hdr body

proceedFromStep6 :: TcpHeader -> S.ByteString -> Sock ()
proceedFromStep6 hdr body =
  do -- XXX skipping URG processing
     -- XXX skipping segment text, as this is processed in deliverSegment
     checkFinBit hdr body
     outputSegments


-- | Make sure that there is space for the incoming segment
checkSequenceNumber :: TcpHeader -> S.ByteString -> Sock ()
checkSequenceNumber hdr body =
  do TcpSocket { .. } <- getTcpSocket

     -- RCV.NXT <= SEG.SEQ + off < RCV.NXT + RCV.WND
     let canReceive off =
           lwRcvNxt tcpIn <= segSeq &&
           segSeq         <  lwRcvNxt tcpIn + fromIntegral (lwRcvWind tcpIn)
           where
           segSeq = tcpSeqNum hdr + off

         len = fromIntegral (S.length body)

         shouldDiscard
           | len == 0  = if lwRcvWind tcpIn == 0
                                   -- SEQ.SEG = RCV.NXT
                            then tcpSeqNum hdr == lwRcvNxt tcpIn
                            else canReceive 0
           | otherwise = canReceive 0 || canReceive (len - 1)

     when (shouldDiscard && all not [tcpAck hdr, tcpUrg hdr, tcpRst hdr]) $
       do unless (tcpRst hdr) ack
          discardAndReturn

-- | Process the presence of the RST bit
checkResetBit :: TcpHeader -> S.ByteString -> Sock ()
checkResetBit hdr body
  | tcpRst hdr =
    do TcpSocket { .. } <- getTcpSocket

       whenState SynReceived $
            -- from an active open
         do when (isNothing tcpParent) $ do flushQueues
                                            closeSocket
            done

       whenStates [Established,FinWait1,FinWait2,CloseWait] $
         do flushQueues
            closeSocket
            done

       whenStates [Closing,LastAck,TimeWait] $
         do closeSocket
            done

  | otherwise  = return ()

checkSynBit :: TcpHeader -> S.ByteString -> Sock ()
checkSynBit hdr body
  | tcpSyn hdr = whenStates [SynReceived,Established,FinWait1,FinWait2
                            ,CloseWait,Closing,LastAck,TimeWait] $
    do tcp <- getTcpSocket

       when (tcpSeqNum hdr `inRcvWnd` tcp) $
         do flushQueues
            closeSocket
            done

  | otherwise  = return ()

checkAckBit :: TcpHeader -> S.ByteString -> Sock ()
checkAckBit hdr body
  | tcpAck hdr =
    do whenState SynReceived $
         do tcp <- getTcpSocket
            if tcpSndUna tcp <= tcpAckNum hdr && tcpAckNum hdr < tcpRcvNxt tcp
               then establishConnection
               else rst hdr

       whenState FinWait1 $
         do tcp <- getTcpSocket
            when (nothingOutstanding tcp) (setState FinWait2)

       -- XXX no way to acknowledge the user's close request from FinWait2

       whenState Closing $
         do tcp <- getTcpSocket
            when (nothingOutstanding tcp) (setState TimeWait)

       whenState LastAck $
         do tcp <- getTcpSocket
            when (nothingOutstanding tcp) $ do closeSocket
                                               done

       whenState TimeWait $
         do ack
            set2MSL mslTimeout

  | otherwise  = discardAndReturn

checkFinBit :: TcpHeader -> S.ByteString -> Sock ()
checkFinBit hdr body
  | tcpFin hdr =
    do whenStates [Closed,Listen,SynSent]
         discardAndReturn

  | otherwise  = return ()


-- XXX flesh this out a bit more, as this doesn't cover everything.
flushQueues :: Sock ()
flushQueues  = modifyTcpSocket_ $ \ tcp -> tcp
  { tcpOut = clearRetransmit (tcpOut tcp)
  }


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
deliverSegment :: TcpHeader -> S.ByteString -> Sock ()
deliverSegment hdr body = do
  TcpSocket { .. } <- getTcpSocket

  -- they ack'd something that we haven't sent yet
  when (tcpAckNum hdr > tcpSndNxt) $ do ack
                                        discardAndReturn

  -- handle data segment acknowledgments
  when (isAck hdr) $ do handleAck hdr
                        outputSegments

  -- process a message, if there was data attached.  in the case that there is
  -- no room in the buffer, the original socket state will be returned,
  -- preventing any ACK from being sent.
  when (S.length body > 0) $ do
    mb <- modifyTcpSocket (handleData hdr body)
    case mb of
      Just wakeup -> outputS (putStrLn "try again" >> tryAgain wakeup)
      Nothing     -> return ()

  -- XXX this doesn't check that the sequence numbers aren't being reused to
  -- update the window

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
  return $ trace ("bytes queued: " ++ show (isJust wakeup)) (wakeup, tcp')
  where
  (segs,win') = incomingPacket hdr body (tcpIn tcp0)
  tcp         = tcp0 { tcpIn = win' }
  bytes       = L.fromChunks (map inBody (F.toList segs))


-- | Handle an ACK to a sent data segment.
handleAck :: TcpHeader -> Sock ()
handleAck hdr = do
  now <- inTcp time
  modifyTcpSocket_ (updateAck now)
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

  shutdown
  -- setState CloseWait

  -- technically, we go to CloseWait now, but we'll transition out immediately,
  -- as we don't wait for user confirmation
  setState CloseWait

  finAck
  setState LastAck

-- | Setup the 2MSL timer, and enter the TIME_WAIT state.
enterTimeWait :: Sock ()
enterTimeWait  = do
  set2MSL mslTimeout
  setState TimeWait

createConnection :: IP4 -> TcpHeader -> Sock TcpSocket
createConnection ip4 hdr =
  do let parent = listenSocketId (tcpDestPort hdr)
     isn <- inTcp initialSeqNum
     tcp <- getTcpSocket
     return (emptyTcpSocket (tcpWindow hdr) (windowScale hdr))
       { tcpParent      = Just parent
       , tcpSocketId    = incomingSocketId ip4 hdr
       , tcpState       = SynReceived
       , tcpIss         = isn
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

establishConnection :: Sock ()
establishConnection  =
  do mb <- inParent popAcceptor
     case mb of
       Just k  -> do sid <- tcpSocketId `fmap` getTcpSocket
                     outputS (k sid)
                     setState Established
                     notify True

       -- no one available to accept the connection, close it
       Nothing -> do finAck
                     setState FinWait1
                     outputS (putStrLn "no one accepting...")
                     done


-- Buffer Delivery -------------------------------------------------------------

-- | Fill up the remote window with segments.
outputSegments :: Sock ()
outputSegments  = do
  now <- inTcp time
  (ws,segs) <- modifyTcpSocket (genSegments now)
  F.mapM_ outputSegment segs
  unless (null ws) (outputS (mapM_ tryAgain ws))

-- | Take data from the output buffer, and turn it into segments.  When no data
-- was written, a wakeup action is returned to signal the user thread to try
-- again.
genSegments :: POSIXTime -> TcpSocket -> (([Wakeup],OutSegments),TcpSocket)
genSegments now tcp0 = loop [] Seq.empty tcp0
  where
  loop ws segs tcp
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

      return (loop (addWakeup mbWakeup) (segs Seq.|> seg) tcp')
    where

    addWakeup Nothing  =   ws
    addWakeup (Just w) = w:ws

    done | not (Seq.null segs) = ((ws,segs),tcp)
         | otherwise           = ((ws,segs),tcp)


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
