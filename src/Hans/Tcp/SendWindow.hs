{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Hans.Tcp.SendWindow (
    Window(),
    emptyWindow,
    sndNxt, setSndNxt,
    sndUna,
    sndWnd,
    queueSegment,
    retransmitTimeout,
    ackSegment,
    handleSack,
  ) where

import Hans.Lens
import Hans.Tcp.Packet

import qualified Data.ByteString.Lazy as L
import           Data.Int (Int64)
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
  Segment { segRightEdge = tcpSegNextAckNum segHeader (fromIntegral (L.length segBody))
          , segSACK      = False
          , .. }

-- | The length of the whole segment, including syn/fin.
segLen :: Getting r Segment Int64
segLen  = to $ \ Segment { .. } ->
  fromIntegral (tcpSegLen segHeader (fromIntegral (L.length segBody)))

-- | The sequence number of the frame.
--
-- INVARIANT: the sequence number must always be greater than or equal to the
-- current sequence number, and less than the value of the right edge of the
-- segment.
leftEdge :: Lens' Segment TcpSeqNum
leftEdge f seg@Segment { segHeader = hdr@TcpHeader { .. }, .. } =
  fmap update (f tcpSeqNum)
  where

  update sn
      -- only update if the sequence number can actually trim the packet
    | sn <= tcpSeqNum = seg
    | otherwise       =
      -- account syn counting for one
      let len = fromTcpSeqNum (sn - tcpSeqNum)
          (hdr',len') | view tcpSyn hdr = (set tcpSyn False hdr,len - 1)
                      | otherwise       = (hdr,len)

      -- NOTE: the header gets the unmodified sequence number, but the data gets
      -- the adjusted version, as the adjusted 
      in Segment { segHeader = hdr' { tcpSeqNum = sn } -- NOTE: use old sn here
                 , segBody   = L.drop len' segBody
                 , .. }


{-# INLINE leftEdge #-}


-- | The sequence number of the first byte AFTER this segment.
--
-- INVARIANT: the sequence number must always be less than or equal to the
-- current right edge sequence number, and greater than or equal to the left
-- edge.
rightEdge :: Getting r Segment TcpSeqNum
rightEdge  = to segRightEdge
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
data Window = Window { wRetransmitQueue :: !Segments

                     , wSndAvail        :: !Int64
                       -- ^ The effective window

                     , wSndNxt          :: !TcpSeqNum
                     , wSndWnd          :: !TcpSeqNum
                     }


emptyWindow :: TcpSeqNum -> TcpSeqNum -> Window
emptyWindow wSndNxt wSndWnd =
  Window { wRetransmitQueue = []
         , wSndAvail        = fromTcpSeqNum wSndWnd
         , .. }


-- | The value of SND.NXT.
--
-- NOTE: SND.UNA <= SND.NXT < SND.UNA + SND.WND
sndNxt :: Getting r Window TcpSeqNum
sndNxt  = to wSndNxt


-- | Only sets the value of SND.NXT when the retransmit queue is empty.
setSndNxt :: TcpSeqNum -> Window -> (Window, Bool)
setSndNxt nxt win
  | null (wRetransmitQueue win) = (win { wSndNxt = nxt }, True)
  | otherwise                   = (win, False)


-- | The value of SND.WND.
sndWnd :: Getting r Window TcpSeqNum
sndWnd  = to wSndWnd


-- | The value of SND.UNA -- the left-edge of the send window.
sndUna :: Getting r Window TcpSeqNum
sndUna  = to $ \ Window { .. } ->
  case wRetransmitQueue of
    seg : _ -> view leftEdge seg
    []      -> wSndNxt


-- | Returns the new send window, as well as boolean indicating whether or not
-- the retransmit timer needs to be started.
queueSegment :: (TcpSeqNum -> TcpHeader) -> L.ByteString
             -> Window -> (Window,Maybe (Bool,TcpHeader,L.ByteString))
queueSegment mkHdr body win
  | wSndAvail win == 0 = (win,Nothing)
  | otherwise          = (win',Just (startRTO,hdr,trimmedBody))
  where

  hdr         = mkHdr (wSndNxt win)

  trimmedBody = L.take (wSndAvail win) body
  seg         = mkSegment hdr trimmedBody

  win' = win { wRetransmitQueue = wRetransmitQueue win ++ [seg]
             , wSndAvail        = wSndAvail win - view segLen seg }

  -- start the retransmit timer when the queue was empty.
  startRTO = null (wRetransmitQueue win)


-- | A retransmit timer has gone off: reset the sack bit on all segments in the
-- queue, and return the segment at the left edge of the window, if it exists.
retransmitTimeout :: Window -> (Window,Maybe (TcpHeader,L.ByteString))
retransmitTimeout win = (win', mbSeg)
  where
  win'  = win { wRetransmitQueue = map (set sack False) (wRetransmitQueue win) }
  mbSeg = case wRetransmitQueue win' of
            Segment { .. } : _ -> Just (segHeader,segBody)
            _                  -> Nothing


-- | Remove all segments of the send window that occur before this sequence
-- number, and increase the size of the available window.
ackSegment :: TcpSeqNum -> Window -> Window
ackSegment ack win =
  win { wRetransmitQueue = go (wRetransmitQueue win)
      , wSndAvail        = wSndAvail win + fromTcpSeqNum (ack - view sndUna win)
      , wSndNxt          = ack }
  where

  go (seg:rest)
      -- this segment is acknowledged by the ack
    | view rightEdge seg <= ack = go rest

      -- the ack falls in the middle of this segment
    | view leftEdge  seg <= ack = set leftEdge ack seg : rest

  -- either the segment queue was empty, or there were segments that had not
  -- been acknowledged
  go segs = segs


-- Selective ACK ---------------------------------------------------------------

-- | Process a sack option, and return the updated window, and the segments that
-- are missing from the remote window.
handleSack :: [SackBlock] -> Window -> (Window,[(TcpHeader,L.ByteString)])
handleSack blocks win =
  let win' = processSackBlocks blocks win
   in (win', sackRetransmit win')

-- | All segments that have not been selectively acknowledged. This can be used
-- when replying to a duplicate ack that contains a SACK option, after the
-- option has been processed. NOTE: this still doesn't remove the packets from
-- the queue, it just means that we know what parts to retransmit.
sackRetransmit :: Window -> [(TcpHeader,L.ByteString)]
sackRetransmit Window { .. } =
  [ (segHeader,segBody) | Segment { .. } <- wRetransmitQueue, not segSACK ]


-- | Mark segments that have been acknowledged through the SACK option.
processSackBlocks :: [SackBlock] -> Window -> Window
processSackBlocks blocks Window { .. } =
  Window { wRetransmitQueue = go wRetransmitQueue (sortBy (comparing sbLeft) blocks)
         , .. }
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
