{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Hans.Tcp.SendWindow (
    SendWindow(),
    emptySendWindow,
    ackSegment,
    sackSegment,
  ) where

import Hans.Lens
import Hans.Tcp.Packet

import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import           Data.List (sortBy)
import qualified Data.Monoid as M
import           Data.Ord (comparing)


-- Segments --------------------------------------------------------------------

data Segment = Segment { segHeader    :: !TcpHeader
                       , segRightEdge :: !TcpSeqNum -- ^ Cached right edge
                       , segBody      :: !L.ByteString
                       , segSACK      :: !Bool
                       } deriving (Show)

mkSegment :: TcpHeader -> L.ByteString -> Segment
mkSegment segHeader segBody =
  Segment { segRightEdge = tcpSeqNum segHeader + fromIntegral (L.length segBody)
          , segSACK      = False
          , .. }

-- | The sequence number of the frame.
--
-- INVARIANT: the sequence number must always be greater than or equal to the
-- current sequence number, and less than the value of the right edge of the
-- segment.
leftEdge :: Lens' Segment TcpSeqNum
leftEdge f seg@Segment { segHeader = TcpHeader { .. }, .. } =
  fmap update (f tcpSeqNum)
  where
  update sn
    | sn == tcpSeqNum = seg
    | otherwise       = 
      Segment { segHeader = TcpHeader { tcpSeqNum = sn, .. }
              , segBody   = L.drop (fromTcpSeqNum (sn - tcpSeqNum)) segBody
              , .. }
{-# INLINE leftEdge #-}


-- | The sequence number of the first byte AFTER this segment.
--
-- INVARIANT: the sequence number must always be less than or equal to the
-- current right edge sequence number, and greater than or equal to the left
-- edge.
rightEdge :: Lens' Segment TcpSeqNum
rightEdge f seg@Segment { segHeader = hdr@TcpHeader { .. }, .. } =
  fmap update (f segRightEdge)
  where
  update sn
    | sn == segRightEdge = seg
    | otherwise          =
      Segment { segHeader    = hdr
              , segRightEdge = sn
              , segBody      = L.take (fromTcpSeqNum (sn - tcpSeqNum)) segBody
              , .. }
{-# INLINE rightEdge #-}


-- | The SACK flag for this segment. This flag is turned TRUE when the segment
-- has been wholly acknowledged through the SACK option on an incoming ACK.
sack :: Lens' Segment Bool
sack f seg@Segment { .. } =
  fmap update (f segSACK)
  where
  update b | b == segSACK = seg
           | otherwise    = Segment { segSACK = b, .. }


-- Send Window -----------------------------------------------------------------

type Segments = [Segment]

-- | This structure holds bookkeeping variables for the remote end's receive
-- window, as well as the retransmit queue.
data SendWindow = SendWindow { swRetransmitQueue :: !Segments
                             }


emptySendWindow :: SendWindow
emptySendWindow  = SendWindow { swRetransmitQueue = []
                              }


-- | Remove all segments of the send window that occur before this sequence
-- number.
ackSegment :: TcpSeqNum -> SendWindow -> SendWindow
ackSegment ack SendWindow { .. } =
  SendWindow { swRetransmitQueue = go swRetransmitQueue }
  where

  go (seg:rest)
      -- this segment is acknowledged by the ack
    | view rightEdge seg <= ack = go rest

      -- the ack falls in the middle of this segment
    | view leftEdge seg <= ack = set leftEdge ack seg : rest

  -- either the segment queue was empty, or there were segments that had not
  -- been acknowledged
  go segs = segs


-- | Mark segments that have been acknowledged through the SACK option.
sackSegment :: [(TcpSeqNum,TcpSeqNum)] -> SendWindow -> SendWindow
sackSegment ranges SendWindow { .. } =
  SendWindow { swRetransmitQueue = go swRetransmitQueue (sortBy (comparing fst) ranges) }
  where

  go queue@(seg:segs) rs@((l,r):rest)

      -- segment falls within the block
    | segWithin seg l r = set sack True seg : go segs rs

      -- segment begins after the block
    | view leftEdge seg >= r = go queue rest

      -- segment ends before the block
    | otherwise = seg : go segs rs

  go segs _ = segs


-- | True when the segment falls wholly within the range given.
segWithin :: Segment -> TcpSeqNum -> TcpSeqNum -> Bool
segWithin seg l r = view leftEdge seg >= l && view rightEdge seg < r
  -- Remember that since SACK blocks define the right edge as being the first
  -- sequence number of the /next/ block, we use strict less-than for the
  -- comparison of the right edge.
