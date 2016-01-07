{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Tcp.Output (
    -- * Output
    routeTcp,
    sendTcp,

    -- ** With a TCB
    sendWithTcb,
    sendAck,

    -- $notes
  ) where

import           Hans.Checksum (finalizeChecksum,extendChecksum)
import           Hans.Device.Types (Device(..),ChecksumOffload(..),txOffload)
import           Hans.Lens (view,set)
import           Hans.Network
import           Hans.Serialize (runPutPacket)
import           Hans.Tcp.Packet
                     (TcpHeader(..),putTcpHeader,emptyTcpHeader,tcpAck)
import qualified Hans.Tcp.RecvWindow as Recv
import qualified Hans.Tcp.SendWindow as Send
import           Hans.Tcp.Tcb
import           Hans.Types

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import           Data.IORef (readIORef,atomicModifyIORef')
import           Data.Int (Int64)
import           Data.Serialize.Put (putWord16be)
import           Data.Word (Word32)


-- Sending with a TCB ----------------------------------------------------------

-- | Send a single ACK immediately.
sendAck :: NetworkStack -> Tcb -> IO ()
sendAck ns tcb =
  do _ <- sendWithTcb ns tcb (set tcpAck True emptyTcpHeader) L.empty
     return ()

-- | Send a segment and queue it in the remote window. The number of bytes that
-- were sent is returned.
sendWithTcb :: NetworkStack -> Tcb -> TcpHeader -> L.ByteString -> IO (Maybe Int64)
sendWithTcb ns Tcb { .. } hdr body =
  do recvWindow <- readIORef tcbRecvWindow
     let mkHdr seqNum = hdr { tcpSeqNum     = seqNum
                            , tcpAckNum     = view Recv.rcvNxt recvWindow
                            , tcpDestPort   = tcbRemotePort
                            , tcpSourcePort = tcbLocalPort }

     mbRes <- atomicModifyIORef' tcbSendWindow (Send.queueSegment mkHdr body)

     case mbRes of

       Just (startRT,hdr',body') ->
         do when startRT $ atomicModifyIORef' tcbTimers
                         $ \ tt -> (resetRetransmit tt, ())
            _ <- sendTcp ns tcbRouteInfo tcbRemote hdr' body'
            return (Just (L.length body'))

       Nothing ->
            return Nothing



-- Primitive Send --------------------------------------------------------------

-- | Send outgoing tcp segments, with a route calculation.
--
-- See note "No Retransmit Queue" ("Hans.Tcp.Output#no-retransmit-queue").
routeTcp :: Network addr
         => NetworkStack -> Device
         -> addr -> addr -> TcpHeader -> L.ByteString -> IO Bool
routeTcp ns dev src dst hdr payload
  | L.length payload > fromIntegral (maxBound :: Word32) =
    return False

  | otherwise =
    do mbRoute <- findNextHop ns (Just dev) (Just src) dst
       case mbRoute of
         Just ri ->
           do let bytes = renderTcpPacket (view txOffload dev) src dst hdr payload
              sendDatagram ns ri dst PROT_TCP bytes
              return True

         Nothing ->
              return False


-- | Lowest-level output function for TCP.
--
-- See note "No Retransmit Queue" ("Hans.Tcp.Output#no-retransmit-queue").
sendTcp :: Network addr
        => NetworkStack
        -> RouteInfo addr -> addr -> TcpHeader -> L.ByteString -> IO Bool
sendTcp ns ri dst hdr payload
  | L.length payload >= fromIntegral (maxBound :: Word32) =
    return False

  | otherwise =
    do let bytes = renderTcpPacket (view txOffload ri) (riSource ri) dst hdr payload
       sendDatagram ns ri dst PROT_TCP bytes

       return True


-- | Render out a tcp packet, calculating the checksum when the device requires
-- it.
renderTcpPacket :: Network addr
                => ChecksumOffload -> addr -> addr -> TcpHeader -> L.ByteString
                -> L.ByteString
renderTcpPacket ChecksumOffload { .. } src dst hdr body
  | coTcp     = bytes
  | otherwise = beforeCS `L.append` csBytes
  where
  bytes  = runPutPacket 20 40 body (putTcpHeader hdr)

  cs     = finalizeChecksum
         $ extendChecksum bytes
         $ pseudoHeader src dst PROT_TCP (fromIntegral (L.length bytes))

  beforeCS = L.take 16 bytes
  csBytes  = runPutPacket 2 0 (L.drop 18 bytes) (putWord16be cs)


-- $notes
-- #no-retransmit-queue#
--
-- = No Retransmit Queue
-- This function will not record entries in the retransmit queue, and is
-- responsible only for output to a lower layer.
