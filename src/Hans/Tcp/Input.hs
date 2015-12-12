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
import Hans.Tcp.Output (routeTcp,sendTcp)
import Hans.Tcp.Packet
import Hans.Tcp.Tcb
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

     tryActive ns dev src' dst' hdr payload

-- | A single case for an incoming segment.
type InputCase = NetworkStack -> Device -> Addr -> Addr -> TcpHeader
              -> S.ByteString -> Hans ()

-- | Process incoming segments that are destined for an active connection.
tryActive :: InputCase
tryActive ns dev src dst hdr payload =
  do mbActive <- io (lookupActive ns src (tcpSourcePort hdr) dst (tcpDestPort hdr))
     case mbActive of
       Just tcb -> handleActive ns dev src dst hdr payload tcb
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
             -> Device -> Addr -> Addr -> TcpHeader -> S.ByteString
             -> Tcb -> Hans ()

handleActive ns dev src dst hdr payload tcb =
  do undefined


-- Listening Connections -------------------------------------------------------

-- | Respond to a segment directed at a socket in the Listen state. This
-- implements the LISTEN case from pages 64-67 of RFC793.
handleListening :: NetworkStack
                -> Device -> Addr -> Addr -> TcpHeader -> S.ByteString
                -> ListenTcb -> Hans ()

handleListening ns dev remote local hdr payload tcb =
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
       do createChildTcb ns dev remote local hdr tcb
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
     io $ do child <- newTcb ri (tcpDestPort hdr) remote (tcpSourcePort hdr) SynReceived
             iss   <- nextIss parent

             atomicWriteIORef (tcbRcvNxt child) (tcpSeqNum hdr + 1)
             atomicWriteIORef (tcbIrs    child) (tcpSeqNum hdr    )
             atomicWriteIORef (tcbIss    child)  iss

             synAck <- mkSynAck child hdr
             _      <- sendTcp ns ri remote synAck L.empty

             atomicWriteIORef (tcbSndNxt child) (iss + 1)
             atomicWriteIORef (tcbSndUna child)  iss

             registerActive ns remote (tcpSourcePort hdr) local (tcpDestPort hdr) child


-- TimeWait Connections --------------------------------------------------------

handleTimeWait :: NetworkStack -> TcpHeader -> S.ByteString -> TimeWaitTcb -> Hans ()
handleTimeWait ns hdr payload tcb =
  do unless (sequenceNumberValid (twRcvNxt tcb) (twRcvWnd tcb) hdr payload) $
       do sndNxt <- io (readIORef (twSndNxt tcb))
          ack    <- io (mkAck sndNxt (twRcvNxt tcb) hdr)
          _      <- io (sendTcp ns (twRouteInfo tcb) (twDest tcb) ack L.empty)
          escape

     when (view tcpRst hdr) $
       do io (deleteTimeWait ns tcb)
          escape

     when (view tcpSyn hdr) $
       do rst <- io (mkRst hdr)
          _   <- io (sendTcp ns (twRouteInfo tcb) (twDest tcb) rst L.empty)
          io (deleteTimeWait ns tcb)
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


-- Sequence Numbers ------------------------------------------------------------

-- | This is the check described on page 68 of RFC793, which checks that data
-- falls within the expected receive window.
sequenceNumberValid :: TcpSeqNum  -- ^ RCV.NXT
                    -> TcpSeqNum  -- ^ RCV.WND
                    -> TcpHeader
                    -> S.ByteString
                    -> Bool

sequenceNumberValid rcvNxt rcvWnd TcpHeader { .. } seg

  | segLen == 0 = or [ rcvWnd == 0 && tcpSeqNum == rcvNxt
                     , inWindow tcpSeqNum ]

  | otherwise   = rcvWnd > 0
               && or [ inWindow tcpSeqNum
                     , inWindow (tcpSeqNum + fromIntegral segLen - 1) ]

  where
  segLen = S.length seg

  inWindow = withinWindow rcvNxt (rcvNxt + rcvWnd)


-- Utilities -------------------------------------------------------------------

-- | @<SEQ=SEG.ACK><CTL=RST>@
mkRst :: TcpHeader -> IO TcpHeader
mkRst TcpHeader { .. } =
     return $ set tcpRst True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = tcpAckNum
                             }
{-# INLINE mkRst #-}


-- | @<SEQ=0><ACK=SEG.SEQ+SEG.LEN><CTL=RST,ACK>@
mkRstAck :: TcpHeader -> S.ByteString -> IO TcpHeader
mkRstAck hdr @ TcpHeader { .. } payload =
     return $ set tcpRst True
            $ set tcpAck True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = 0
                             , tcpAckNum     = tcpSegNextAckNum hdr (S.length payload)
                             }
{-# INLINE mkRstAck #-}


-- | @<SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>@
mkSynAck :: Tcb -> TcpHeader -> IO TcpHeader
mkSynAck Tcb { .. } TcpHeader { .. } =
  do iss <- readIORef tcbIss
     ack <- readIORef tcbRcvNxt

     return $ set tcpSyn True
            $ set tcpAck True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = iss
                             , tcpAckNum     = ack
                             }
{-# INLINE mkSynAck #-}


mkAck :: TcpSeqNum    -- ^ SEG.SEQ
      -> TcpSeqNum    -- ^ SEG.ACK
      -> TcpHeader
      -> IO TcpHeader
mkAck segSeq segAck TcpHeader { .. } =
     return $ set tcpAck True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = segSeq
                             , tcpAckNum     = segAck
                             }
{-# INLINE mkAck #-}

