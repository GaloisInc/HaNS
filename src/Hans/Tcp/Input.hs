{-# LANGUAGE PatternSynonyms #-}

module Hans.Tcp.Input (
    processTcp4
  ) where

import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Device.Types (Device(..),DeviceConfig(..))
import Hans.IP4 (IP4,ip4PseudoHeader,pattern IP4_PROT_TCP)
import Hans.Lens
import Hans.Monad (Hans,escape,decode',dropPacket,io)
import Hans.Tcp.Output (routeTcp4,sendTcp4)
import Hans.Tcp.Packet
import Hans.Types

import           Control.Monad (unless,when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (atomicWriteIORef)
import           Data.Time.Clock.POSIX (getPOSIXTime)


-- | Process incoming tcp segments.
processTcp4 :: NetworkStack -> Device -> IP4 -> IP4 -> S.ByteString -> Hans ()
processTcp4 ns dev src dst bytes =
  do -- make sure that the checksum is valid
     let checksum = finalizeChecksum $ extendChecksum bytes
                                     $ ip4PseudoHeader src dst IP4_PROT_TCP
                                     $ S.length bytes
     unless (dcChecksumOffload (devConfig dev) || checksum == 0)
            (dropPacket (devStats dev))

     (hdr,payload) <- decode' (devStats dev) getTcpHeader bytes

     incomingSegment ns dev src dst hdr payload


incomingSegment :: NetworkStack
                -> Device -> IP4 -> IP4 -> TcpHeader -> S.ByteString -> Hans ()

incomingSegment ns dev src dst hdr payload =
  do tcb <- io (findTcb4 ns src (tcpSourcePort hdr) dst (tcpDestPort hdr))

     case tcb of

       Closed      -> respondClosed  ns dev src dst hdr (S.length payload)
       Listen ltcb -> respondListen  ns dev src dst hdr payload ltcb
       SynSent tcb -> respondSynSent ns dev src dst hdr payload tcb
       _           -> escape


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
respondClosed :: NetworkStack
              -> Device -> IP4 -> IP4 -> TcpHeader -> Int -> Hans ()
respondClosed ns dev src dst hdr payloadLen
  | view tcpRst hdr = escape
  | otherwise       = do _ <- io (routeTcp4 ns dev dst src hdr' L.empty)
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
              -> Device -> IP4 -> IP4 -> TcpHeader -> S.ByteString
              -> ListenTcb -> Hans ()
respondListen ns dev src dst hdr payload ltcb =
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

          _ <- io (routeTcp4 ns dev src dst hdr' L.empty)
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

     when (view tcpSyn hdr) $
       do mbRoute <- io (lookupRoute ns dst)
          nxt     <- case mbRoute of

                       Just (src',nxt,dev')
                         | src == src' && dev == dev' -> return nxt

                       _ -> escape

          io $ do tcb <- newTcb dev src dst nxt
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
                  _ <- sendTcp4 ns dev src dst nxt hdr' L.empty

                  atomicWriteIORef (tcbSndNxt tcb) (iss + 1)
                  atomicWriteIORef (tcbSndUna tcb)  iss

                  registerSocket ns
                      (Conn4 src (tcpSourcePort hdr) dst (tcpDestPort hdr))
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

respondSynSent :: NetworkStack
               -> IP4 -> IP4 -> TcpHeader -> L.ByteString -> Tcb -> Hans ()
respondSynSent ns src dst hdr payload tcb =

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

  do when (view tcpAck hdr) $
       do iss    <- io (readIORef (tcbIss    tcb))
          sndNxt <- io (readIORef (tcbSndNxt tcb))
          when (tcpAckNum hdr <= iss || tcpAckNum > sndNxt) $
            do unless (view tcpRst hdr) $ io $
                 do next <- readIORef (tcbNext4 tcb)
                    let hdr' = set tcpRst True
                             $ emptyTcpHeader { tcpSourcePort = tcpDestPort hdr
                                              , tcpDestPort   = tcpSourcePort hdr
                                              , tcpSeqNum     = tcpAckNum hdr
                                              }
                    _ <- sendTcp4 ns src dst next hdr' L.empty
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
          unless (view tcpAck hdr && sndUna <= tcpAckNum hdr
                                  && tcpAckNum hdr <= sndNxt) $
            do registerSocket key Closed

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
--
--   fifth, if neither of the SYN or RST bits is set then drop the
--   segment and return.
