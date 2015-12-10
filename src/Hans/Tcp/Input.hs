{-# LANGUAGE PatternSynonyms #-}

module Hans.Tcp.Input (
    processTcp
  ) where

import Hans.Addr (Addr,NetworkAddr(..))
import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Device.Types (Device(..),ChecksumOffload(..),rxOffload)
import Hans.Lens (view,set)
import Hans.Monad (Hans,escape,decode',dropPacket,io)
import Hans.Network
import Hans.Tcp.Output (routeTcp,sendTcp)
import Hans.Tcp.Packet
import Hans.Types

import           Control.Monad (unless,when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (atomicWriteIORef,readIORef)


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

     let src' = toAddr src
     let dst' = toAddr dst

     state <- io (findTcb ns src' (tcpSourcePort hdr) dst' (tcpDestPort hdr))
     case state of
       Closed      -> respondClosed  ns dev src' dst' hdr (S.length payload)
       Listen ltcb -> respondListen  ns dev src' dst' hdr payload ltcb
       SynSent tcb -> respondSynSent ns dev src' dst' hdr payload tcb
       _           -> checkSequenceNumber ns dev src' dst' hdr payload state


-- Page 64 ---------------------------------------------------------------------

-- If the state is CLOSED (i.e., TCB does not exist) then
--
--   all data in the incoming segment is discarded.  An incoming
--   segment containing a RST is discarded.  An incoming segment not
--   containing a RST causes a RST to be sent in response.  The
--   acknowledgment and sequence field values are selected to make the
--   reset sequence acceptable to the TCP that sent the offending
--   segment.
--
--   If the ACK bit is on,
--
--     <SEQ=SEG.ACK><CTL=RST>
--
--   If the ACK bit is off, sequence number zero is used,
--
--     <SEQ=0><ACK=SEG.SEQ+SEG.LEN><CTL=RST,ACK>
--
--   Return.
respondClosed :: Network addr
              => NetworkStack
              -> Device -> addr -> addr -> TcpHeader -> Int -> Hans ()
respondClosed ns dev src dst hdr payloadLen
  | view tcpRst hdr = escape
  | otherwise       = do _ <- io (routeTcp ns dev dst src hdr' L.empty)
                         escape
  where
  hdr' | view tcpAck hdr = set tcpRst True
                         $ emptyTcpHeader
                               { tcpSourcePort = tcpDestPort hdr
                               , tcpDestPort   = tcpSourcePort hdr
                               , tcpSeqNum     = tcpAckNum hdr
                               }

       | otherwise       = set tcpRst True
                         $ set tcpAck True
                         $ emptyTcpHeader
                               { tcpSourcePort = tcpDestPort hdr
                               , tcpDestPort   = tcpSourcePort hdr
                               , tcpSeqNum     = 0
                               , tcpAckNum     = tcpSegNextAckNum hdr payloadLen
                               }

-- If the state is LISTEN then
--
respondListen :: NetworkStack
              -> Device -> Addr -> Addr -> TcpHeader -> S.ByteString
              -> ListenTcb -> Hans ()
respondListen ns dev src dst hdr _payload ltcb =
--   first check for an RST
--
--     An incoming RST should be ignored.  Return.
  do when (view tcpRst hdr) escape

--   second check for an ACK
--
--     Any acknowledgment is bad if it arrives on a connection still in
--     the LISTEN state.  An acceptable reset segment should be formed
--     for any arriving ACK-bearing segment.  The RST should be
--     formatted as follows:
--
--       <SEQ=SEG.ACK><CTL=RST>
--
--     Return.
     when (view tcpAck hdr) $
       do let hdr' = set tcpRst True
                   $ emptyTcpHeader { tcpDestPort   = tcpSourcePort hdr
                                    , tcpSourcePort = tcpDestPort hdr
                                    , tcpSeqNum     = tcpAckNum hdr
                                    , tcpAckNum     = 0
                                    }

          _ <- io (routeTcp ns dev src dst hdr' L.empty)
          escape

--   third check for a SYN
--
--     If the SYN bit is set, check the security.  If the
--     security/compartment on the incoming segment does not exactly
--     match the security/compartment in the TCB then send a reset and
--     return.
--
--       <SEQ=SEG.ACK><CTL=RST>
--
--    If the SEG.PRC is greater than the TCB.PRC then if allowed by
--    the user and the system set TCB.PRC<-SEG.PRC, if not allowed
--    send a reset and return.
--
--      <SEQ=SEG.ACK><CTL=RST>
--
--    If the SEG.PRC is less than the TCB.PRC then continue.
--
--    Set RCV.NXT to SEG.SEQ+1, IRS is set to SEG.SEQ and any other
--    control or text should be queued for processing later.  ISS
--    should be selected and a SYN segment sent of the form:
--
--      <SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>
--
--    SND.NXT is set to ISS+1 and SND.UNA to ISS.  The connection
--    state should be changed to SYN-RECEIVED.  Note that any other
--    incoming control or data (combined with SYN) will be processed
--    in the SYN-RECEIVED state, but processing of SYN and ACK should
--    not be repeated.  If the listen was not fully specified (i.e.,
--    the foreign socket was not fully specified), then the
--    unspecified fields should be filled in now.

     -- XXX currently not queueing any control or text present
     when (view tcpSyn hdr) $
       do mbRoute <- io (findNextHop ns (Just dev) (Just dst) src)
          ri      <- case mbRoute of
                       Just ri -> return ri
                       _       -> escape

          io $ do tcb <- newTcb ri (tcpDestPort hdr) src (tcpSourcePort hdr)
                  iss <- nextIss ltcb

                  atomicWriteIORef (tcbRcvNxt tcb) (tcpSeqNum hdr + 1)
                  atomicWriteIORef (tcbIrs    tcb) (tcpSeqNum hdr    )
                  atomicWriteIORef (tcbIss    tcb)  iss

                  let hdr' = set tcpSyn True
                           $ set tcpAck True
                           $ emptyTcpHeader
                             { tcpSourcePort = tcpDestPort hdr
                             , tcpDestPort   = tcpSourcePort hdr
                             , tcpSeqNum     = iss
                             , tcpAckNum     = tcpSeqNum hdr + 1
                             }
                  _ <- sendTcp ns ri src hdr' L.empty

                  atomicWriteIORef (tcbSndNxt tcb) (iss + 1)
                  atomicWriteIORef (tcbSndUna tcb)  iss

                  registerSocket ns
                      (Conn src (tcpSourcePort hdr) dst (tcpDestPort hdr))
                      (SynReceived tcb)

--   fourth other text or control
--
--     Any other control or text-bearing segment (not containing SYN)
--     must have an ACK and thus would be discarded by the ACK
--     processing.  An incoming RST segment could not be valid, since
--     it could not have been sent in response to anything sent by this
--     incarnation of the connection.  So you are unlikely to get here,
--     but if you do, drop the segment, and return.

     escape




-- If the state is SYN-SENT then

respondSynSent :: NetworkStack -> Device
               -> Addr -> Addr -> TcpHeader -> S.ByteString -> Tcb -> Hans ()
respondSynSent ns _dev src dst hdr payload tcb =

--   first check the ACK bit
--
--     If the ACK bit is set
--
--       If SEG.ACK =< ISS, or SEG.ACK > SND.NXT, send a reset (unless
--       the RST bit is set, if so drop the segment and return)
--
--         <SEQ=SEG.ACK><CTL=RST>
--
--       and discard the segment.  Return.

  do sndNxt <- io (readIORef (tcbSndNxt tcb))
     when (view tcpAck hdr) $
       do iss <- io (readIORef (tcbIss    tcb))
          when (tcpAckNum hdr <= iss || tcpAckNum hdr > sndNxt) $
            do unless (view tcpRst hdr) $ io $
                 do let hdr' = set tcpRst True
                             $ emptyTcpHeader { tcpSourcePort = tcpDestPort hdr
                                              , tcpDestPort   = tcpSourcePort hdr
                                              , tcpSeqNum     = tcpAckNum hdr
                                              }
                    _ <- sendTcp ns (tcbRouteInfo tcb) src hdr' L.empty
                    return ()

               escape

--       If SND.UNA =< SEG.ACK =< SND.NXT then the ACK is acceptable.
--
--   second check the RST bit
--   If the RST bit is set
--
--       If the ACK was acceptable then signal the user "error:
--       connection reset", drop the segment, enter CLOSED state,
--       delete TCB, and return.  Otherwise (no ACK) drop the segment
--       and return.

     when (view tcpRst hdr) $
       do sndUna <- io (readIORef (tcbSndUna tcb))
          unless (view tcpAck hdr && sndUna        <= tcpAckNum hdr
                                  && tcpAckNum hdr <= sndNxt)
               -- XXX: this closes the socket in the tcp state, but doesn't
               -- notify the user
              (io (registerSocket ns (error "need key") Closed))
          escape

--   third check the security and precedence
--
--     If the security/compartment in the segment does not exactly
--     match the security/compartment in the TCB, send a reset
--
--       If there is an ACK
--
--         <SEQ=SEG.ACK><CTL=RST>
--
--       Otherwise
--
--         <SEQ=0><ACK=SEG.SEQ+SEG.LEN><CTL=RST,ACK>
--
--     If there is an ACK
--
--       The precedence in the segment must match the precedence in the
--       TCB, if not, send a reset
--
--         <SEQ=SEG.ACK><CTL=RST>
--
--     If there is no ACK
--
--       If the precedence in the segment is higher than the precedence
--       in the TCB then if allowed by the user and the system raise
--       the precedence in the TCB to that in the segment, if not
--       allowed to raise the prec then send a reset.
--
--         <SEQ=0><ACK=SEG.SEQ+SEG.LEN><CTL=RST,ACK>
--
--       If the precedence in the segment is lower than the precedence
--       in the TCB continue.
--
--     If a reset was sent, discard the segment and return.
--
--   fourth check the SYN bit
--
--     This step should be reached only if the ACK is ok, or there is
--     no ACK, and it the segment did not contain a RST.
--
--     If the SYN bit is on and the security/compartment and precedence
--     are acceptable then, RCV.NXT is set to SEG.SEQ+1, IRS is set to
--     SEG.SEQ.  SND.UNA should be advanced to equal SEG.ACK (if there
--     is an ACK), and any segments on the retransmission queue which
--     are thereby acknowledged should be removed.
--
--     If SND.UNA > ISS (our SYN has been ACKed), change the connection
--     state to ESTABLISHED, form an ACK segment
--
--       <SEQ=SND.NXT><ACK=RCV.NXT><CTL=ACK>
--
--     and send it.  Data or controls which were queued for
--     transmission may be included.  If there are other controls or
--     text in the segment then continue processing at the sixth step
--     below where the URG bit is checked, otherwise return.
--
--     Otherwise enter SYN-RECEIVED, form a SYN,ACK segment
--
--       <SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>
--
--     and send it.  If there are other controls or text in the
--     segment, queue them for processing after the ESTABLISHED state
--     has been reached, return.

     -- XXX make sure to check the retransmit queue here
     when (view tcpSyn hdr) $ io $
       do let rcvNxt = tcpSeqNum hdr + 1
          atomicWriteIORef (tcbRcvNxt tcb) rcvNxt
          atomicWriteIORef (tcbIrs    tcb) (tcpSeqNum hdr)

          let sndUna = tcpAckNum hdr
          atomicWriteIORef (tcbSndUna tcb) sndUna
          iss <- readIORef (tcbIss tcb)

          if sndUna > iss
             then
               do registerSocket ns (error "need key") (Established tcb)
                  -- XXX need to ack any queued data
                  let hdr' = set tcpSyn True
                           $ set tcpAck True
                           $ emptyTcpHeader { tcpSourcePort = tcpDestPort hdr
                                            , tcpDestPort   = tcpSourcePort hdr
                                            , tcpSeqNum     = iss
                                            , tcpAckNum     = rcvNxt }
                  _ <- sendTcp ns (tcbRouteInfo tcb) src hdr' L.empty

                  -- XXX what other controls should be considered here?
                  unless (S.null payload) (step6UrgCheck ns src dst hdr tcb payload)

             else
               do registerSocket ns (error "need key") (SynReceived tcb)
                  let hdr' = set tcpSyn True
                           $ set tcpAck True
                           $ emptyTcpHeader { tcpSourcePort = tcpDestPort hdr
                                            , tcpDestPort   = tcpSourcePort hdr
                                            , tcpSeqNum     = iss
                                            , tcpAckNum     = rcvNxt }

                  -- XXX need to queue additional control/text for processing
                  -- after entering Established
                  _ <- sendTcp ns (tcbRouteInfo tcb) src hdr' L.empty

                  return ()

--   fifth, if neither of the SYN or RST bits is set then drop the
--   segment and return.
     escape


-- Page 68 ---------------------------------------------------------------------

-- Otherwise,
--
--   first check sequence number
--
--     SYN-RECEIVED STATE
--     ESTABLISHED STATE
--     FIN-WAIT-1 STATE
--     FIN-WAIT-2 STATE
--     CLOSE-WAIT STATE
--     CLOSING STATE
--     LAST-ACK STATE
--     TIME-WAIT STATE
--
--       Segments are processed in sequence.  Initial tests on arrival
--       are used to discard old duplicates, but further processing is
--       done in SEG.SEQ order.  If a segment's contents straddle the
--       boundary between old and new, only the new parts should be
--       processed.
--
--       There are four cases for the acceptability test for an incoming
--       segment:
--
--       Segment Receive  Test
--       Length  Window
--       ------- -------  -------------------------------------------
--
--          0       0     SEG.SEQ = RCV.NXT
--
--          0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND
--
--         >0       0     not acceptable
--
--         >0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND
--                     or RCV.NXT =< SEG.SEQ+SEG.LEN-1 < RCV.NXT+RCV.WND
--
--       If the RCV.WND is zero, no segments will be acceptable, but
--       special allowance should be made to accept valid ACKs, URGs and
--       RSTs.
--
--       If an incoming segment is not acceptable, an acknowledgment
--       should be sent in reply (unless the RST bit is set, if so drop
--       the segment and return):
--
--         <SEQ=SND.NXT><ACK=RCV.NXT><CTL=ACK>
--
--       After sending the acknowledgment, drop the unacceptable segment
--       and return.
--
--       In the following it is assumed that the segment is the idealized
--       segment that begins at RCV.NXT and does not exceed the window.
--       One could tailor actual segments to fit this assumption by
--       trimming off any portions that lie outside the window (including
--       SYN and FIN), and only processing further if the segment then
--       begins at RCV.NXT.  Segments with higher begining sequence
--       numbers may be held for later processing.

checkSequenceNumber :: NetworkStack -> Device
                    -> Addr -> Addr -> TcpHeader -> S.ByteString -> Tcb
                    -> Hans ()


-- -----------------------------------------------------------------------------

step6UrgCheck = undefined
