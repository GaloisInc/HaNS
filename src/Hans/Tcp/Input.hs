{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Tcp.Input (
    processTcp
  ) where

import Hans.Addr (Addr,NetworkAddr(..))
import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Config (config)
import Hans.Device.Types (Device(..),ChecksumOffload(..),rxOffload)
import Hans.Lens (view,set)
import Hans.Monad (Hans,escape,decode',dropPacket,io,callCC)
import Hans.Nat.Forward (tryForwardTcp)
import Hans.Network
import Hans.Tcp.Message
import Hans.Tcp.Output (routeTcp,queueTcp,queueAck,queueWithTcb,queueTcp)
import Hans.Tcp.Packet
import Hans.Tcp.RecvWindow
           (sequenceNumberValid,recvSegment)
import Hans.Tcp.SendWindow (ackSegment,nullWindow)
import Hans.Tcp.Tcb
import Hans.Types

import           Control.Monad (unless,when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (atomicModifyIORef',atomicWriteIORef,readIORef)
import           Data.Maybe (isJust)
import           Data.Time.Clock (UTCTime,NominalDiffTime,getCurrentTime)


-- | Process incoming tcp segments.
processTcp :: Network addr
           => NetworkStack -> Device -> addr -> addr -> S.ByteString -> Hans ()
processTcp ns dev src dst bytes =
  do -- make sure that the checksum is valid
     let checksum = finalizeChecksum $ extendChecksum bytes
                                     $ pseudoHeader src dst PROT_TCP
                                     $ S.length bytes
     unless (coTcp (view rxOffload dev) || checksum == 0)
            (dropPacket (devStats dev))

     (hdr,payload) <- decode' (devStats dev) getTcpHeader bytes

     let remote = toAddr src
     let local  = toAddr dst

     tryActive ns dev remote local hdr payload

-- | A single case for an incoming segment.
type InputCase = NetworkStack -> Device -> Addr -> Addr -> TcpHeader
              -> S.ByteString -> Hans ()

-- | Process incoming segments that are destined for an active connection.
tryActive :: InputCase
tryActive ns dev src dst hdr payload =
  do mbActive <- io (lookupActive ns src (tcpSourcePort hdr) dst (tcpDestPort hdr))
     case mbActive of
       Just tcb -> handleActive ns dev hdr payload tcb
       Nothing  -> tryListening ns dev src dst hdr payload
{-# INLINE tryActive #-}


-- | Process incoming segments that are destined for a listening connection.
tryListening :: InputCase
tryListening ns dev src dst hdr payload =
  do mbListening <- io (lookupListening ns dst (tcpDestPort hdr))
     case mbListening of
       Just tcb -> handleListening ns dev src dst hdr payload tcb
       Nothing  -> tryTimeWait ns dev src dst hdr payload
{-# INLINE tryListening #-}


-- | Process incoming segments that are destined for a connection in TimeWait.
tryTimeWait :: InputCase
tryTimeWait ns dev remote local hdr payload =
  do mbTimeWait <- io (lookupTimeWait ns remote (tcpSourcePort hdr) local (tcpDestPort hdr))
     case mbTimeWait of
       Just tcb -> handleTimeWait ns hdr payload tcb
       Nothing  -> tryForward ns dev remote local hdr payload
{-# INLINE tryTimeWait #-}


-- | Process incoming segments to a forwarded port.
tryForward :: InputCase
tryForward ns dev remote local hdr payload =
  do mbHdr <- io (tryForwardTcp ns local remote hdr)
     case mbHdr of

       Just (ri,dst,hdr') -> io $
         do _ <- queueTcp ns ri dst hdr' (L.fromStrict payload)
            return ()

       Nothing            -> handleClosed ns dev remote local hdr payload
{-# INLINE tryForward #-}



-- Active Connections ----------------------------------------------------------

handleActive :: NetworkStack
             -> Device -> TcpHeader -> S.ByteString -> Tcb -> Hans ()

handleActive ns dev hdr payload tcb =
     -- XXX it would be nice to add header prediction here
  do updateTimestamp tcb hdr payload

     whenState tcb SynSent (handleSynSent ns dev hdr payload tcb)

     -- page 69
     -- check sequence numbers
     mbSegs <- io (atomicModifyIORef' (tcbRecvWindow tcb) (recvSegment hdr payload))
     case mbSegs of

       Nothing ->
         do unless (view tcpRst hdr) $ io $
              do _ <- queueWithTcb ns tcb (set tcpAck True emptyTcpHeader) L.empty
                 return ()

       Just segs ->
         do now <- io getCurrentTime
            handleActiveSegs ns tcb now segs

     escape


-- | Update the internal timestamp. This follows the algorithm described on
-- pages 15 and 16 of RFC-1323.
updateTimestamp :: Tcb -> TcpHeader -> S.ByteString -> Hans ()
updateTimestamp Tcb { .. } hdr payload =
  do TcbConfig { .. } <- io (readIORef tcbConfig)

     when tcUseTimestamp $
       case findTcpOption OptTagTimestamp hdr of

         -- update TS.recent when Last.ACK.sent falls within the segment, and
         -- there isn't an outstanding delayed ack registered.
         Just (OptTimestamp val _) ->
           do lastAckSent <- io (readIORef tcbLastAckSent)
              delayed     <- io (readIORef tcbNeedsDelayedAck)
              let end = tcpSegNextAckNum hdr (S.length payload)
              when (not delayed && withinWindow (tcpSeqNum hdr) end lastAckSent)
                   (io (atomicWriteIORef tcbTSRecent val))

         -- when the timestamp is missing, but we're using timestamps, drop the
         -- segment, unless it was an RST
         _ | view tcpRst hdr -> return ()
           | otherwise       -> escape


-- | At this point, the list of segments is contiguous, and starts at the old
-- value of RCV.NXT. RCV.NXT has been advanced to point at the end of the
-- segment list.
handleActiveSegs :: NetworkStack -> Tcb -> UTCTime -> [(TcpHeader,S.ByteString)]
                 -> Hans ()
handleActiveSegs ns tcb now = go
  where
  go []                   = return ()
  go ((hdr,payload):segs) =
    do callCC $ \ k ->
         do let continue = k ()

            -- page 70 and page 71
            -- check RST/check SYN
            when (view tcpRst hdr || view tcpSyn hdr) $
              do io $ do when (view tcpSyn hdr) $
                           do let rst = set tcpRst True emptyTcpHeader
                              _ <- queueWithTcb ns tcb rst L.empty
                              return ()

                         -- when the tcb was passively opened, this will free its
                         -- accept queue slot.
                         setState tcb Closed

                         closeActive ns tcb

                 escape

            -- page 71
            -- skipping security/precedence

            -- page 72
            -- check ACK
            unless (view tcpAck hdr) continue

            -- Reset idle timeout
            io $ atomicModifyIORef' (tcbTimers tcb) resetIdleTimer

            -- update the send window
            mbAck <- io $
              do mbAck <- atomicModifyIORef' (tcbSendWindow tcb)
                              (ackSegment (view config ns) now (tcpAckNum hdr))
                 handleRTTMeasurement tcb mbAck

            state <- io (getState tcb)
            case state of

              SynReceived ->
                case mbAck of
                  Just True  -> io (setState tcb Established)
                  Just False -> return ()
                  Nothing    -> do let rst = set tcpRst True emptyTcpHeader
                                   _ <- io (queueWithTcb ns tcb rst L.empty)
                                   continue

              FinWait1 ->
                case mbAck of
                  Just True ->
                    do io (setState tcb FinWait2)
                       io (processFinWait2 ns tcb)
                       continue

                  _ -> continue

              FinWait2 ->
                case mbAck of
                  Just True ->
                    do io (processFinWait2 ns tcb)
                       continue

                  _ -> continue

              Closing ->
                case mbAck of
                  Just True -> enterTimeWait ns tcb
                  _         -> continue

              LastAck ->
                case mbAck of
                  Just True ->
                    do io (setState tcb Closed)
                       io (closeActive ns tcb)
                       escape

                  _ -> continue

              -- TimeWait processing is done in handleTimeWait

              -- CloseWait | Established
              _ -> return ()

            -- page 73
            -- check URG

            -- page 74
            -- process the segment text
            -- XXX: we're ignoring PSH for now, just making the data immediately
            -- available
            unless (S.null payload) $ io $
              do signalDelayedAck tcb
                 queueBytes payload tcb

            -- page 75
            -- check FIN
            when (view tcpFin hdr) $
              do -- send an ACK to the FIN
                 _ <- io (queueAck ns tcb)

                 state' <- io (getState tcb)
                 case state' of

                   SynReceived -> io (setState tcb CloseWait)
                   Established -> io (setState tcb CloseWait)

                   FinWait1 ->
                     case mbAck of
                       Just True -> enterTimeWait ns tcb
                       _         -> io (setState tcb Closing)

                   FinWait2 -> enterTimeWait ns tcb

                   _ -> continue

            continue

       go segs


-- | Processing for the FinWait2 state, when the retransmit queue is known to be
-- empty. (Page 73 of RFC-793)
processFinWait2 :: NetworkStack -> Tcb -> IO ()
processFinWait2 _ns Tcb { .. } =
  do win <- readIORef tcbSendWindow

     when (nullWindow win) $
       do -- XXX acknowledge the user's close request
          return ()


-- | Invalidate, and remove the active 'Tcb', replacing it with a 'TimeWaitTcb'
-- that was derived from it.
enterTimeWait :: NetworkStack -> Tcb -> Hans ()
enterTimeWait ns tcb =
  do io (closeActive tcb)
     tw <- io (mkTimeWaitTcb tcb)
     io (registerTimeWait ns tw)
     escape


-- | Apply an RTT measurement to the timers in a tcb, and return the relevant
-- information about the state of the send queue.
handleRTTMeasurement :: Tcb -> Maybe (Bool, Maybe NominalDiffTime)
                     -> IO (Maybe Bool)
handleRTTMeasurement Tcb { .. } mb =
  case mb of
    Just (b, Just rtt) ->
      do atomicModifyIORef' tcbTimers (calibrateRTO rtt)
         return (Just b)

    Just (b,_) ->
         return (Just b)

    Nothing ->
         return Nothing


-- Half-open Connections -------------------------------------------------------

-- | Handle incoming packets destined for a tcb that's in the SYN-SENT state.
handleSynSent :: NetworkStack -> Device -> TcpHeader -> S.ByteString -> Tcb
              -> Hans ()
handleSynSent ns _dev hdr _payload tcb =
  do -- page 66
     iss <- io (readIORef (tcbIss tcb))
     when (view tcpAck hdr) $
       do sndNxt <- getSndNxt tcb

          when (tcpAckNum hdr <= iss || tcpAckNum hdr > sndNxt) $
            do when (view tcpRst hdr) escape

               rst <- io (mkRst hdr)
               _   <- io (queueTcp ns (tcbRouteInfo tcb) (tcbRemote tcb) rst L.empty)
               escape

     -- page 66/67
     when (view tcpRst hdr) $
       do when (view tcpAck hdr) $ io $
            -- NOTE: the ACK must have been acceptable at this point, as we
            -- would have not made it this far otherwise.
            do setState tcb Closed
               deleteActive ns tcb

          escape

     -- no security/precedence currently

     -- page 67/68
     when (view tcpSyn hdr) $
       do let rcvNxt = tcpSeqNum hdr + 1

          res <- io (setRcvNxt rcvNxt tcb)
          -- should never happen, but if a segment was queued in the receive
          -- window, mucking about with RCV.NXT will cause processing to abort
          --
          -- XXX: maybe there's a better way to encode advancing RCV.NXT over
          -- the SYN?
          unless res escape

          io (atomicWriteIORef (tcbIrs tcb) (tcpSeqNum hdr))

          sndUna <- io $
            if view tcpAck hdr
               then do now <- getCurrentTime
                       mb  <- atomicModifyIORef' (tcbSendWindow tcb)
                                  (ackSegment (view config ns) now (tcpAckNum hdr))
                       _   <- handleRTTMeasurement tcb mb
                       return (tcpAckNum hdr)

               else getSndUna tcb

          when (sndUna > iss) $
            do -- XXX: include any queued data/controls
               _ <- io (queueAck ns tcb)
               io (setState tcb Established)

               -- XXX: not processing additional data/controls from the ack
               escape

          io (setState tcb SynReceived)
          let synAck = set tcpSyn True
                     $ set tcpAck True emptyTcpHeader
          _ <- io (queueWithTcb ns tcb synAck L.empty)

          -- XXX: not queueing any additional data
          escape

     -- page 68
     -- at this point, neither syn or rst were set, so drop the segment
     escape


-- Listening Connections -------------------------------------------------------

-- | Respond to a segment directed at a socket in the Listen state. This
-- implements the LISTEN case from pages 64-67 of RFC793.
handleListening :: NetworkStack
                -> Device -> Addr -> Addr -> TcpHeader -> S.ByteString
                -> ListenTcb -> Hans ()

handleListening ns dev remote local hdr _payload tcb =
     -- ignore all incoming RST packets (page 64)
  do when (view tcpRst hdr)
          escape

     -- send a RST for incoming ACK packets (page 64)
     when (view tcpAck hdr) $
       do hdr' <- io (mkRst hdr)
          _    <- io (routeTcp ns dev local remote hdr' L.empty)
          escape

     -- we don't enforce security in HaNS (page 65)
     -- XXX should we be managing precedence?
     -- XXX we're not queueing any other control/data that arrived
     when (view tcpSyn hdr) $
       do canAccept <- io (decrSynBacklog ns)
          unless canAccept (rejectSyn ns dev remote local hdr)

          createChildTcb ns dev remote local hdr tcb
          escape

     -- drop anything else that made it this far (page 65)
     escape


-- | Create a child TCB from a listening one.
createChildTcb :: NetworkStack -> Device -> Addr -> Addr -> TcpHeader -> ListenTcb
               -> Hans ()
createChildTcb ns dev remote local hdr parent =

     -- cache routing information for the child, and fail if there's no valid
     -- route
  do mbRoute <- io (findNextHop ns (Just dev) (Just local) remote)
     ri      <- case mbRoute of
                  Just ri -> return ri
                  Nothing -> rejectSyn ns dev remote local hdr

     -- reserve a spot in the accept queue
     canAccept <- io (reserveSlot parent)
     unless canAccept (rejectSyn ns dev remote local hdr)

     -- construct a new tcb, and initialize it as specified on (page 65)
     (added,child) <- io $
       do iss   <- nextIss ns local (tcpDestPort hdr) remote (tcpSourcePort hdr)
          child <- createChild ns iss parent ri remote hdr
                   (\_ _ -> incrSynBacklog ns)
                   (\_ s -> when (s == SynReceived) (incrSynBacklog ns))
          added <- registerActive ns child
          return (added,child)

     -- if we couldn't add the entry to the active connections, reject it
     unless added $
       do io (releaseSlot parent)
          rejectSyn ns dev remote local hdr

     -- queueing a SYN/ACK in the send window will advance SND.NXT
     -- automatically
     io (processSynOptions child hdr)
     let synAck = set tcpSyn True
                $ set tcpAck True emptyTcpHeader
     _ <- io (queueWithTcb ns child synAck L.empty)

     return ()


-- | Determine which options should be sent in the SYN,ACK response.
processSynOptions :: Tcb -> TcpHeader -> IO ()
processSynOptions Tcb { .. } hdr =
  do case findTcpOption OptTagTimestamp hdr of

       -- the timestamp option is requested by the remote side
       Just (OptTimestamp val 0) ->
         do atomicModifyIORef' tcbConfig $ \ TcbConfig { .. } ->
                (TcbConfig { tcUseTimestamp = True, .. }, ())
            atomicWriteIORef tcbTSRecent val

       -- no timestamp option, disable timestamps
       _ -> atomicModifyIORef' tcbConfig $ \ TcbConfig { .. } ->
                (TcbConfig { tcUseTimestamp = False, .. }, ())

     -- XXX: in the future, enable SACK here
     -- XXX: in the future, enable Window Scale here

     return ()


-- | Reject the SYN by sending an RST, drop the segment and return.
rejectSyn :: NetworkStack -> Device -> Addr -> Addr -> TcpHeader -> Hans a
rejectSyn ns dev remote local hdr =
  do hdr' <- io (mkRst hdr)
     _    <- io (routeTcp ns dev local remote hdr' L.empty)
     escape


-- TimeWait Connections --------------------------------------------------------

handleTimeWait :: NetworkStack -> TcpHeader -> S.ByteString -> TimeWaitTcb -> Hans ()
handleTimeWait ns hdr payload tcb =
     -- page 69
  do (rcvNxt,rcvRight) <- getRecvWindow tcb
     unless (isJust (sequenceNumberValid rcvNxt rcvRight hdr payload)) $
       do unless (view tcpRst hdr) $ io $
            do ack <- mkAck (twSndNxt tcb) rcvNxt (tcpDestPort hdr) (tcpSourcePort hdr)
               _   <- queueTcp ns (twRouteInfo tcb) (twRemote tcb) ack L.empty
               return ()
          escape

     -- page 70
     when (view tcpRst hdr) $
       do io (deleteTimeWait ns tcb)
          escape

     -- page 71
     when (view tcpSyn hdr) $
       do rst <- io (mkRst hdr)
          _   <- io (queueTcp ns (twRouteInfo tcb) (twRemote tcb) rst L.empty)
          io (deleteTimeWait ns tcb)
          escape

     -- page 73
     when (not (view tcpAck hdr)) escape

     -- page 74
     -- ignore the URG bit

     -- page 75
     -- ignore the segment text

     -- page 75/76
     -- process the FIN bit
     when (view tcpFin hdr) $
       do rcvNxt' <- io $
              atomicModifyIORef' (twRcvNxt tcb) $ \ i ->
                  let i' = i + 1 + fromIntegral (S.length payload)
                   in (i', i')

          ack <- io (mkAck (twSndNxt tcb) rcvNxt' (tcpDestPort hdr) (tcpSourcePort hdr))
          _   <- io (queueTcp ns (twRouteInfo tcb) (twRemote tcb) ack L.empty)

          io (resetTimeWait ns tcb)
          escape

     escape


-- Closed Connections ----------------------------------------------------------

-- | Respond to a segment that was directed at a closed port. This implements
-- the CLOSED case on page 64 of RFC793
handleClosed :: NetworkStack
             -> Device -> Addr -> Addr -> TcpHeader -> S.ByteString -> Hans ()
handleClosed ns dev remote local hdr payload =
  do when (view tcpRst hdr) escape

     rst <- io $ if view tcpAck hdr then mkRst    hdr
                                    else mkRstAck hdr payload
     _   <- io (routeTcp ns dev local remote rst L.empty)
     escape
