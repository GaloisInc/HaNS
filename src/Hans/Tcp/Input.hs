{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Tcp.Input (
    processTcp
  ) where

import Hans.Addr (Addr,NetworkAddr(..))
import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Device.Types (Device(..),ChecksumOffload(..),rxOffload)
import Hans.Lens (view,set)
import Hans.Monad (Hans,escape,decode',dropPacket,io)
import Hans.Network
import Hans.Tcp.Message
import Hans.Tcp.Output (routeTcp,sendTcp,sendAck,sendWithTcb)
import Hans.Tcp.Packet
import Hans.Tcp.RecvWindow
           (sequenceNumberValid,recvSegment)
import Hans.Tcp.SendWindow (ackSegment)
import Hans.Tcp.Tcb
import Hans.Types

import           Control.Monad (unless,when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (atomicModifyIORef',atomicWriteIORef,readIORef)
import           Data.Maybe (isJust)


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
       Nothing  -> handleClosed ns dev remote local hdr payload
{-# INLINE tryTimeWait #-}


-- Active Connections ----------------------------------------------------------

handleActive :: NetworkStack
             -> Device -> TcpHeader -> S.ByteString -> Tcb -> Hans ()

handleActive ns dev hdr payload tcb =
     -- XXX it would be nice to add header prediction here
  do whenState tcb SynSent (handleSynSent ns dev hdr payload tcb)

     -- page 69
     -- check sequence numbers
     mbSegs <- io (atomicModifyIORef' (tcbRecvWindow tcb) (recvSegment hdr payload))
     case mbSegs of

       Nothing ->
         do unless (view tcpRst hdr) $ io $
              do (rcvNxt,_) <- getRecvWindow tcb
                 sndNxt     <- getSndNxt tcb
                 ack        <- mkAck sndNxt rcvNxt (tcpDestPort hdr) (tcpSourcePort hdr)
                 _          <- sendTcp ns (tcbRouteInfo tcb) (tcbRemote tcb) ack L.empty
                 return ()

       Just segs ->
         do mapM_ (handleActiveSeg ns dev tcb) segs
            escape


-- | At this point, the list of segments is contiguous, and starts at the old
-- value of RCV.NXT. RCV.NXT has been advanced to point at the end of the
-- segment list.
handleActiveSeg :: NetworkStack -> Device -> Tcb -> (TcpHeader,S.ByteString)
                -> Hans ()
handleActiveSeg ns _dev tcb (hdr,payload) =
  do -- page 70 and page 71
     -- check RST/check SYN
     when (view tcpRst hdr || view tcpSyn hdr) $
       do io $ do when (view tcpSyn hdr) $
                    do let rst = set tcpRst True emptyTcpHeader
                       _ <- sendWithTcb ns tcb rst L.empty
                       return ()

                  -- NOTE: we don't set the state of a passive socket to Closed,
                  -- as that might provoke action by the listening socket.
                  state <- getState tcb
                  if state == SynReceived && isJust (tcbParent tcb)
                     then incrSynBacklog ns
                     else setState tcb Closed

                  deleteActive ns tcb

          escape

     -- page 71
     -- skipping security/precedence

     -- page 72
     -- check ACK
     when (view tcpAck hdr) $
       do undefined


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
               _   <- io (sendTcp ns (tcbRouteInfo tcb) (tcbRemote tcb) rst L.empty)
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
               then do atomicModifyIORef' (tcbSendWindow tcb)
                           $ \win -> (ackSegment (tcpAckNum hdr) win, ())
                       return (tcpAckNum hdr)

               else getSndUna tcb

          when (sndUna > iss) $
            do -- increment the syn backlog if this socket originated with a
               -- listening connection
               when (isJust (tcbParent tcb)) (io (incrSynBacklog ns))

               -- XXX: include any queued data/controls
               io (sendAck ns tcb)
               io (setState tcb Established)

               -- XXX: not processing additional data/controls from the ack
               escape

          io (setState tcb SynReceived)
          synAck <- io (mkSynAck tcb hdr)
          _      <- io (sendTcp ns (tcbRouteInfo tcb) (tcbRemote tcb) synAck L.empty)

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
          unless canAccept escape

          createChildTcb ns dev remote local hdr tcb
          escape

     -- drop anything else that made it this far (page 65)
     escape


-- | Create a child TCB from a listening one.
--
-- XXX this doesn't register the child on the syn queue of the parent
createChildTcb :: NetworkStack -> Device -> Addr -> Addr -> TcpHeader -> ListenTcb
               -> Hans ()
createChildTcb ns dev remote local hdr parent =

     -- cache routing information for the child, and fail if there's no valid
     -- route
  do mbRoute <- io (findNextHop ns (Just dev) (Just local) remote)
     ri      <- case mbRoute of
                  Just ri -> return ri
                  Nothing -> escape

     -- construct a new tcb, and initialize it as specified on (page 65)
     io $ do iss   <- nextIss parent

             child <- newTcb ns (Just parent) iss ri (tcpDestPort hdr) remote
                          (tcpSourcePort hdr) SynReceived
                          -- XXX fix these
                          (\_ -> return ())
                          (\_ -> return ())

             atomicWriteIORef (tcbIrs child) (tcpSeqNum hdr)
             atomicWriteIORef (tcbIss child)  iss

             -- queueing a SYN/ACK in the send window will advance SND.NXT
             -- automatically
             _ <- setSndNxt iss child
             let synAck = set tcpSyn True
                        $ set tcpAck True emptyTcpHeader
             _ <- sendWithTcb ns child synAck L.empty

             registerActive ns remote (tcpSourcePort hdr) local (tcpDestPort hdr) child


-- TimeWait Connections --------------------------------------------------------

handleTimeWait :: NetworkStack -> TcpHeader -> S.ByteString -> TimeWaitTcb -> Hans ()
handleTimeWait ns hdr payload tcb =
     -- page 69
  do (rcvNxt,rcvRight) <- getRecvWindow tcb
     unless (isJust (sequenceNumberValid rcvNxt rcvRight hdr payload)) $
       do unless (view tcpRst hdr) $ io $
            do ack <- mkAck (twSndNxt tcb) rcvNxt (tcpDestPort hdr) (tcpSourcePort hdr)
               _   <- sendTcp ns (twRouteInfo tcb) (twDest tcb) ack L.empty
               return ()
          escape

     -- page 70
     when (view tcpRst hdr) $
       do io (deleteTimeWait ns tcb)
          escape

     -- page 71
     when (view tcpSyn hdr) $
       do rst <- io (mkRst hdr)
          _   <- io (sendTcp ns (twRouteInfo tcb) (twDest tcb) rst L.empty)
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
          _   <- io (sendTcp ns (twRouteInfo tcb) (twDest tcb) ack L.empty)

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
