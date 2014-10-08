{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Hans.Layer.Tcp.Handlers (
    handleIncomingTcp
  , outputSegments
  ) where

import Hans.Address.IP4
import Hans.Layer
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Timers
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Ip4
import Hans.Message.Tcp

import Control.Monad (guard,when,unless,join)
import Data.Bits (bit)
import Data.Int (Int64)
import Data.Maybe (fromMaybe,isJust,isNothing)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F


-- Incoming Packets ------------------------------------------------------------

-- | Process a single incoming tcp packet.
handleIncomingTcp :: IP4Header -> S.ByteString -> Tcp ()
handleIncomingTcp ip4 bytes = do

  let src = ip4SourceAddr ip4
      dst = ip4DestAddr ip4

  guard (validateTcpChecksumIP4 src dst bytes)
  (hdr,body) <- liftRight (parseTcpPacket bytes)

  withConnection' src hdr (segmentArrives src hdr body)
    $ timeWaitConnection src hdr
    $ noConnection src hdr body

noConnection :: IP4 -> TcpHeader -> S.ByteString -> Tcp ()
noConnection src hdr @ TcpHeader { .. } body =
  do output (putStrLn "no connection")
     if | tcpRst    -> return ()
        | tcpAck    -> sendSegment src (mkRst hdr)                    L.empty
        | otherwise -> sendSegment src (mkRstAck hdr (S.length body)) L.empty
     finish


-- Arrival to a TIME_WAIT socket------------------------------------------------

-- | Handle an incoming packet in the TimeWait state
timeWaitConnection :: IP4 -> TcpHeader -> Tcp () -> Tcp ()
timeWaitConnection src hdr noTimeWait =
  do mb <- getTimeWait src hdr
     case mb of
       Just (sid,tcp) -> handleTimeWait sid tcp hdr
       Nothing        -> noTimeWait


handleTimeWait :: SocketId -> TimeWaitSock -> TcpHeader -> Tcp ()
handleTimeWait sid TimeWaitSock { .. } TcpHeader { .. }
  | tcpRst || tcpSyn = removeTimeWait sid
  | tcpAck           = do resetTimeWait2MSL sid

                          let addTimestamp
                                | Just ts <- twTimestamp = setTcpOption (mkTimestamp ts)
                                | otherwise              = id

                              hdr = addTimestamp emptyTcpHeader
                                        { tcpDestPort   = tcpSourcePort
                                        , tcpSourcePort = tcpDestPort
                                        , tcpSeqNum     = twSeqNum
                                          -- advance RCV.NXT over the FIN
                                        , tcpAckNum     = tcpSeqNum + 1
                                        , tcpAck        = True
                                        }

                          -- ACK the retransmitted FIN,ACK
                          when tcpFin (sendSegment (sidRemoteHost sid) hdr L.empty)
  | otherwise        = return ()



-- Segment Arrival -------------------------------------------------------------

{-# INLINE discardAndReturn #-}
discardAndReturn :: Sock a
discardAndReturn  = do outputSegments
                       escape

{-# INLINE done #-}
done :: Sock a
done  = do outputSegments
           escape

segmentArrives :: IP4 -> TcpHeader -> S.ByteString -> Sock ()
segmentArrives src hdr body =
  do shouldDrop <- modifyTcpSocket (updateTimestamp hdr)
     when shouldDrop discardAndReturn

     whenState Closed (inTcp (noConnection src hdr body))

     whenState Listen $
       do when (tcpAck hdr) (rst hdr)

          when (tcpSyn hdr) $ do child <- createConnection src hdr
                                 _     <- withChild child synAck
                                 return ()

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
               modifyTcpSocket_ $ \ sock -> sock
                 { tcpOutMSS      = fromMaybe (tcpInMSS sock) (getMSS hdr)

                   -- clear out, and configure the retransmit buffer
                 , tcpOut         = setSndWind (tcpWindow hdr)
                                  $ setSndWindScale (windowScale hdr)
                                  $ clearRetransmit
                                  $ tcpOut sock

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
                               notify True

                               -- continue at step 6
                               when (tcpUrg hdr) (proceedFromStep6 hdr body)

                       else do setState SynReceived
                               synAck
                               -- XXX queue any additional data for processing
                               -- once Established has been reached

                    done

     -- make sure that the sequence numbers are valid
     checkSequenceNumber hdr body
     checkResetBit hdr
     -- skip security/precidence check
     checkSynBit hdr
     checkAckBit hdr
     proceedFromStep6 hdr body

proceedFromStep6 :: TcpHeader -> S.ByteString -> Sock ()
proceedFromStep6 hdr body =
  do -- XXX skipping URG processing
     processSegmentText hdr body
     checkFinBit hdr


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
checkResetBit :: TcpHeader -> Sock ()
checkResetBit hdr
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

       whenStates [Closing,LastAck] $
         do closeSocket
            done

  | otherwise  = return ()

checkSynBit :: TcpHeader -> Sock ()
checkSynBit hdr
  | tcpSyn hdr = whenStates [SynReceived,Established,FinWait1,FinWait2
                            ,CloseWait,Closing,LastAck] $
    do tcp <- getTcpSocket

       when (tcpSeqNum hdr `inRcvWnd` tcp) $
         do flushQueues
            closeSocket
            done

  | otherwise  = return ()

checkAckBit :: TcpHeader -> Sock ()
checkAckBit hdr
  | tcpAck hdr =
    do whenState SynReceived $
         do tcp <- getTcpSocket
            if tcpSndUna tcp <= tcpAckNum hdr && tcpAckNum hdr <= tcpSndNxt tcp
               then establishConnection
               else rst hdr

       whenStates [Established,FinWait1,FinWait2,CloseWait,Closing] $
         do let TcpHeader { .. } = hdr
            TcpSocket { .. } <- getTcpSocket

            -- if the ack was to something that is outstanding, update SND.UNA,
            -- and filter things out of the retransmit queue
            when (tcpSndUna < tcpAckNum && tcpAckNum <= tcpSndNxt) (handleAck hdr)

            -- if the ack is to something that hasn't been sent yet, drop the
            -- segment
            when (tcpSndNxt < tcpAckNum) discardAndReturn

            whenState FinWait1 $
              do tcp <- getTcpSocket
                 when (nothingOutstanding tcp) (setState FinWait2)

            -- XXX no way to acknowledge the user's close request from FinWait2
            -- whenState FinWait2 $ ...

            whenState Closing $
              do tcp <- getTcpSocket
                 when (nothingOutstanding tcp) enterTimeWait

       whenState LastAck $
         do tcp <- getTcpSocket
            when (nothingOutstanding tcp) $ do closeSocket
                                               done

  | otherwise  = discardAndReturn

processSegmentText :: TcpHeader -> S.ByteString -> Sock ()
processSegmentText hdr body =
  whenStates [Established,FinWait1,FinWait2] $
    do if S.null body
          -- make sure that the the delayed ack flag gets set
          then when (tcpSyn hdr || tcpFin hdr) $ modifyTcpTimers_
                                               $ \ tt -> tt { ttDelayedAck = True }

          -- push the segment through the local window, if it was the next thing
          -- expected, a wakeup action will be returned, notifying a waiting
          -- thread that there's something to read in the input buffer.
          else do mb <- modifyTcpSocket (handleData hdr body)

                  -- notify any waiting threads that there is stuff to read
                  case mb of
                    Just wakeup -> outputS (tryAgain wakeup)
                    Nothing     -> return ()


  -- otherwise, ignore the segment text

checkFinBit :: TcpHeader -> Sock ()
checkFinBit hdr
  | tcpFin hdr =
    do -- do not process the FIN
       whenStates [Closed,Listen,SynSent]
         discardAndReturn

       -- advance RCV.NXT over the FIN
       advanceRcvNxt 1

       flushQueues
       ack

       -- don't emit a FIN here, as that should be done by the user with an
       -- explicit call to `close`

       whenStates [SynReceived,Established] (setState CloseWait)

       -- XXX this needs to check that the FIN was ACKed, instead of just
       -- entering TimeWait
       whenState FinWait1 $ do TcpSocket { .. } <- getTcpSocket
                               if tcpSndNxt <= tcpSndUna
                                  then enterTimeWait
                                  else setState Closing

       whenState FinWait2 enterTimeWait

       -- don't do anything else for the remaining states
       done

  | otherwise  = return ()


-- | Flush all pending outgoing state.
--
-- TODO:
-- * Push all data out of the incoming queue to the incoming buffer
-- * Mark the socket as not accepting any more user data
flushQueues :: Sock ()
flushQueues  =
  do finalizers <- modifyTcpSocket flush
     outputS finalizers
  where
  flush tcp = (fins,tcp')
    where

    -- empty the outgoing buffer, notifying all waiting processes that the send
    -- won't happen
    (fins,out') = flushWaiting (tcpOutBuffer tcp)

    tcp' = tcp { tcpOutBuffer = out'
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
          { ttDelayedAck = or [ not (L.null bytes)
                              , tcpSyn hdr
                              , tcpFin hdr ]
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
  where
  -- using Karns algorithm, only calibrate the RTO if the packet that was ack'd
  -- has not yet been retransmitted.
  updateAck now tcp = case receiveAck hdr (tcpOut tcp) of
    Just (seg,out') ->
      let calibrate | outFresh seg = calibrateRTO now (outTime seg)
                    | otherwise    = id
       in tcp { tcpOut    = out'
              , tcpSndUna = tcpAckNum hdr
              , tcpTimers = calibrate (tcpTimers tcp)
              }
    Nothing -> tcp

-- | Setup the 2MSL timer, and enter the TIME_WAIT state.  We only enter
-- TimeWait if all of our sent data has been ACKed, so it's safe to clear out
-- the retransmit queue here.
enterTimeWait :: Sock ()
enterTimeWait  = do
  modifyTcpSocket_ $ \ tcp -> tcp { tcpOut = clearRetransmit (tcpOut tcp) }
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
     case join mb of
       Just k  -> do sid <- tcpSocketId `fmap` getTcpSocket
                     outputS (k sid)
                     setState Established

       -- no one available to accept the connection, close it
       Nothing -> do finAck
                     setState FinWait1
                     done


-- Buffer Delivery -------------------------------------------------------------

-- | Fill up the remote window with segments.
outputSegments :: Sock ()
outputSegments  = do
  now <- inTcp time
  (ws,segs) <- modifyTcpSocket (genSegments now)
  F.mapM_ outputSegment segs
  unless (null ws) (outputS (F.traverse_ tryAgain ws))


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
