{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Tcp.Output (
    -- * Output
    routeTcp,
    sendTcp,

    -- ** With a TCB
    sendWithTcb,
    sendAck,
    sendFin,
    sendData,
    canSend,

    -- ** From the fast-path
    queueTcp,
    queueWithTcb,
    queueAck,
    responder,

    -- $notes
  ) where

import           Hans.Addr (IP6)
import           Hans.Config (config)
import           Hans.Checksum (finalizeChecksum,extendChecksum)
import           Hans.Device.Types (Device(..),ChecksumOffload(..),txOffload)
import           Hans.Lens (view,set)
import           Hans.Network
import           Hans.Serialize (runPutPacket)
import           Hans.Tcp.Packet
                     (TcpHeader(..),putTcpHeader,emptyTcpHeader,tcpAck,tcpFin
                     ,tcpPsh,TcpOption(..),setTcpOption)
import qualified Hans.Tcp.RecvWindow as Recv
import qualified Hans.Tcp.SendWindow as Send
import           Hans.Tcp.Tcb
import           Hans.Types

import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad (when,forever)
import qualified Data.ByteString.Lazy as L
import           Data.IORef (readIORef,atomicModifyIORef',atomicWriteIORef)
import           Data.Int (Int64)
import           Data.Serialize.Put (putWord16be)
import           Data.Time.Clock (getCurrentTime)
import           Data.Word (Word32)


-- Sending with a TCB ----------------------------------------------------------

-- | Send a single ACK immediately.
sendAck :: NetworkStack -> Tcb -> IO ()
sendAck ns tcb =
  do _ <- sendWithTcb ns tcb (set tcpAck True emptyTcpHeader) L.empty
     return ()

-- | Send a single FIN packet.
sendFin :: NetworkStack -> Tcb -> IO ()
sendFin ns tcb =
  do let hdr = set tcpFin True
             $ set tcpAck True emptyTcpHeader
     _ <- sendWithTcb ns tcb hdr L.empty
     return ()

-- | Send a data segment, potentially sending multiple packets if the send
-- window allows, and the payload is larger than MSS. When the remote window is
-- full, this returns 0.
sendData :: NetworkStack -> Tcb -> L.ByteString -> IO Int64
sendData ns tcb = go 0
  where
  -- technically the MSS could change between sends, so this ensures that we
  -- would pick up that change.
  go acc bytes
    | L.null bytes =
      return acc

    | otherwise =
      do mss <- fromIntegral `fmap` readIORef (tcbMss tcb)
         mb  <- sendWithTcb ns tcb hdr (L.take mss bytes)
         case mb of

           -- when the amount sent was less than mss, we've filled the send
           -- window, and won't be able to send any more.
           Just len | len < mss -> return $! acc + len
                    | otherwise -> let acc' = acc + len
                                    in acc' `seq` go acc' (L.drop len bytes)

           -- the send window is full, return the accumulator
           Nothing -> return acc

  hdr = set tcpAck True
      $ set tcpPsh True
        emptyTcpHeader


-- | Determine if there is any room in the remote window for us to send
-- data.
canSend :: Tcb -> IO Bool
canSend Tcb { .. } =
  (not . Send.fullWindow) `fmap` readIORef tcbSendWindow

-- | Send a segment and queue it in the remote window. The number of bytes that
-- were sent is returned.
sendWithTcb :: NetworkStack -> Tcb -> TcpHeader -> L.ByteString -> IO (Maybe Int64)
sendWithTcb ns Tcb { .. } hdr body =
  do TcbConfig { .. } <- readIORef tcbConfig

     recvWindow <- readIORef tcbRecvWindow

     mbTSecr <- if tcUseTimestamp
                   then Just `fmap` readIORef tcbTSRecent
                   else return Nothing

     let mkHdr tsVal seqNum =
           addTimestamp tsVal mbTSecr
             hdr { tcpSeqNum     = seqNum
                 , tcpAckNum     = if view tcpAck hdr
                                      then view Recv.rcvNxt recvWindow
                                      else 0
                 , tcpDestPort   = tcbRemotePort
                 , tcpSourcePort = tcbLocalPort
                 , tcpWindow     = view Recv.rcvWnd recvWindow
                 }

     -- only enter the retransmit queue if the segment contains data
     now   <- getCurrentTime
     mbRes <- atomicModifyIORef' tcbSendWindow
                  (Send.queueSegment (view config ns) now mkHdr body)
     case mbRes of

       Just (startRT,hdr',body') ->
         do -- clear the delayed ack flag and update Last.ACK.sent, when an ack
            -- is present
            when (view tcpAck hdr') $
              do atomicWriteIORef tcbNeedsDelayedAck False
                 atomicWriteIORef tcbLastAckSent (tcpAckNum hdr')

            -- reset the retransmit timer, if the retransmit queue is now
            -- non-empty
            when startRT (atomicModifyIORef' tcbTimers resetRetransmit)

            -- send the frame
            _ <- sendTcp ns tcbRouteInfo tcbRemote hdr' body'

            -- return how much of the segment was actually delivered
            return (Just (L.length body'))

       Nothing ->
            return Nothing


-- | The presence of a tracked TSecr value controls whether or not we send the
-- timestamp option.
addTimestamp :: Word32 -> Maybe Word32 -> TcpHeader -> TcpHeader
addTimestamp tsVal (Just tsEcr) hdr = setTcpOption (OptTimestamp tsVal tsEcr) hdr
addTimestamp _     _            hdr = hdr


-- Fast-path Sending -----------------------------------------------------------

-- | Responder thread for messages generated in the fast-path.
responder :: NetworkStack -> IO ()
responder ns = forever $
  do msg <- BC.readChan chan
     case msg of
       SendSegment ri dst hdr body ->
         do _ <- sendTcp ns ri dst hdr body
            return ()

       SendWithTcb tcb hdr body ->
         do _ <- sendWithTcb ns tcb hdr body
            return ()

  where
  chan = view tcpQueue ns


-- | Queue an outgoing TCP segment from the fast-path.
--
-- See note "No Retransmit Queue" ("Hans.Tcp.Output#no-retransmit-queue").
queueTcp :: NetworkStack
         -> RouteInfo IP6 -> IP6 -> TcpHeader -> L.ByteString -> IO Bool
queueTcp ns ri dst hdr body =
  BC.tryWriteChan (view tcpQueue ns) $! SendSegment ri dst hdr body


-- | Queue an outgoing TCP segment from the fast-path.
queueWithTcb :: NetworkStack -> Tcb -> TcpHeader -> L.ByteString -> IO Bool
queueWithTcb ns tcb hdr body =
  BC.tryWriteChan (view tcpQueue ns) $! SendWithTcb tcb hdr body


-- | Queue an ACK from the fast-path.
queueAck :: NetworkStack -> Tcb -> IO Bool
queueAck ns tcb = queueWithTcb ns tcb (set tcpAck True emptyTcpHeader) L.empty


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
              sendDatagram ns ri dst False PROT_TCP bytes
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
       sendDatagram ns ri dst False PROT_TCP bytes

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
