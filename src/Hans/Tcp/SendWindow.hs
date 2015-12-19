{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Hans.Tcp.SendWindow (
    SendWindow(),
    emptySendWindow,
    queueSegment,
    retransmitTimeout,
    ackSegment,
    handleSack,
  ) where

import Hans.Lens
import Hans.Tcp.Packet

import qualified Data.ByteString.Lazy as L
import           Data.List (sortBy)
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


-- | Returns the new send window, as well as boolean indicating whether or not
-- the retransmit timer needs to be started.
queueSegment :: TcpHeader -> L.ByteString -> SendWindow -> (SendWindow,Bool)
queueSegment hdr body SendWindow { .. } =
  ( SendWindow { swRetransmitQueue = swRetransmitQueue ++ [mkSegment hdr body]
               , .. }
  , null swRetransmitQueue )


-- | A retransmit timer has gone off: reset the sack bit on all segments in the
-- queue, and return the segment at the left edge of the window, if it exists.
retransmitTimeout :: SendWindow -> (SendWindow,Maybe (TcpHeader,L.ByteString))
retransmitTimeout win = (win', mbSeg)
  where
  win' = SendWindow { swRetransmitQueue = map (set sack False) (swRetransmitQueue win)
                    , .. }

  mbSeg = case swRetransmitQueue win' of
            Segment { .. } : _ -> Just (segHeader,segBody)
            _                  -> Nothing


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


-- Selective ACK ---------------------------------------------------------------

-- | Process a sack option, and return the updated window, and the segments that
-- are missing from the remote window.
handleSack :: [SackBlock] -> SendWindow -> (SendWindow,[(TcpHeader,L.ByteString)])
handleSack blocks win =
  let win' = processSackBlocks blocks win
   in (win', sackRetransmit win')

-- | All segments that have not been selectively acknowledged. This can be used
-- when replying to a duplicate ack that contains a SACK option, after the
-- option has been processed. NOTE: this still doesn't remove the packets from
-- the queue, it just means that we know what parts to retransmit.
sackRetransmit :: SendWindow -> [(TcpHeader,L.ByteString)]
sackRetransmit SendWindow { .. } =
  [ (segHeader,segBody) | Segment { .. } <- swRetransmitQueue, not segSACK ]


-- | Mark segments that have been acknowledged through the SACK option.
processSackBlocks :: [SackBlock] -> SendWindow -> SendWindow
processSackBlocks blocks SendWindow { .. } =
  SendWindow { swRetransmitQueue = go swRetransmitQueue
                                 $ sortBy (comparing sbLeft) blocks }
  where

  go queue@(seg:segs) bs@(SackBlock { .. } :rest)

      -- segment falls within the block
    | segWithin seg sbLeft sbRight = set sack True seg : go segs bs

      -- segment begins after the block
    | view leftEdge seg >= sbRight = go queue rest

      -- segment ends before the block
    | otherwise = seg : go segs bs

  go segs _ = segs


-- | True when the segment falls wholly within the range given.
segWithin :: Segment -> TcpSeqNum -> TcpSeqNum -> Bool
segWithin seg l r = view leftEdge seg >= l && view rightEdge seg < r
  -- Remember that since SACK blocks define the right edge as being the first
  -- sequence number of the /next/ block, we use strict less-than for the
  -- comparison of the right edge.
