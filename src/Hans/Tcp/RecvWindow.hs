{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Hans.Tcp.RecvWindow (
    -- * Receive Window
    RecvWindow(),
    emptyRecvWindow,
    recvSegment,
    recvWindowSize,
    recvWindowNext, setWindowNext,

    -- ** Segments
    Segment(..),

    -- ** Sequence Numbers
    sequenceNumberValid,
  ) where

import           Hans.Tcp.Packet

import qualified Data.ByteString as S


-- Segments --------------------------------------------------------------------

data Segment = Segment { segStart :: !TcpSeqNum
                         -- ^ The first byte occupied by this segment

                       , segEnd :: !TcpSeqNum
                         -- ^ The last byte occupied by this segment

                       , segHdr  :: !TcpHeader
                       , segBody :: !S.ByteString
                       } deriving (Show)

mkSegment :: TcpHeader -> S.ByteString -> Segment
mkSegment segHdr segBody =
  Segment { segStart = tcpSeqNum segHdr
          , segEnd   = tcpSeqNum segHdr + fromIntegral (S.length segBody) - 1
          , .. }

-- | The next segment number, directly after this one.
segNext :: Segment -> TcpSeqNum
segNext Segment { .. } = segEnd + 1

segLen :: Segment -> Int
segLen Segment { .. } = S.length segBody

-- | Drop data off of the front of this segment.
trimSeg :: Int -> Segment -> Maybe Segment
trimSeg len seg@Segment { .. }
  | len <= 0 =
    Just seg

  | len >= S.length segBody =
    Nothing

  | otherwise =
    Just Segment { segStart = segStart + fromIntegral len
                 , segHdr   = segHdr { tcpSeqNum = tcpSeqNum segHdr + fromIntegral len }
                 , segBody  = S.drop len segBody
                 , .. }

-- | Resolve overlap between two segments. It's assumed that the two segments do
-- actually overlap.
resolveOverlap :: Segment -> Segment -> [Segment]
resolveOverlap a b =
  case trimSeg (fromTcpSeqNum (segEnd x - segStart y)) x of
    Just x' -> [x',y]
    Nothing -> error "resolveOverlap: invariant violated"
  where
  (x,y) | segStart a < segStart b = (a,b) -- a overlaps b
        | otherwise               = (b,a) -- b overlaps a


-- Receive Window --------------------------------------------------------------

-- | The receive window.
--
-- INVARIANTS:
--
--  1. All segments in rwSegments are within the window defined by rwRcvNxt and
--     rwRcvWnd
--  2. The segments in rwSegments should not overlap
--
data RecvWindow = RecvWindow { rwSegments :: ![Segment]
                               -- ^ Out of order segments.

                             , rwRcvNxt :: !TcpSeqNum
                               -- ^ Left-edge of the receive window

                             , rwRcvWnd :: !TcpSeqNum

                               -- ^ Current size of the receive window

                             , rwMax :: !TcpSeqNum
                               -- ^ Maximum size of the receive window

                             } deriving (Show)

emptyRecvWindow :: TcpSeqNum -> Int -> RecvWindow
emptyRecvWindow rwRcvNxt maxWin =
  RecvWindow { rwSegments = []
             , rwRcvWnd   = rwRcvNxt + rwMax
             , .. }
  where
  rwMax = fromIntegral maxWin

recvWindowSize :: RecvWindow -> TcpSeqNum
recvWindowSize RecvWindow { .. } = rwRcvWnd

recvWindowNext :: RecvWindow -> TcpSeqNum
recvWindowNext RecvWindow { .. } = rwRcvNxt

-- | Only sets RCV.NXT when the segment queue is empty. Returns 'True' when the
-- value has been successfully changed.
setWindowNext :: TcpSeqNum -> RecvWindow -> (RecvWindow,Bool)
setWindowNext rcvNxt rw
  | null (rwSegments rw) = (rw { rwRcvNxt = rcvNxt }, True)
  | otherwise            = (rw, False)

-- | Check an incoming segment, and queue it in the receive window. The boolean
-- parameter on the second element of the pair is True when the segment received
-- was valid.
recvSegment :: TcpHeader -> S.ByteString -> RecvWindow
            -> (RecvWindow, (Bool,[Segment]))

recvSegment hdr body rw

  | Just seg <- sequenceNumberValid (rwRcvNxt rw) (rwRcvWnd rw) hdr body =
    let (rw',segs) = addSegment seg rw
     in (rw',(True,segs))

    -- drop the invalid frame
  | otherwise =
    (rw, (False,[]))


-- | Add a validated segment to the receive window, and return 
addSegment :: Segment -> RecvWindow -> (RecvWindow,[Segment])
addSegment seg rw

    -- The new segment falls right at the beginning of the receive window
  | segStart seg == rwRcvNxt rw =
    advanceWindow seg rw

    -- As addSegment should only be called with the results of
    -- sequenceNumberValid, the only remaining case to consider is that the
    -- segment falls somewhere else within the window.
  | otherwise =
    (insertOutOfOrder seg rw, [])


-- | Use this segment to advance the window, which may unblock zero or more out
-- of order segments. The list returned is always non-empty, as it includes the
-- segment that's given.
advanceWindow :: Segment -> RecvWindow -> (RecvWindow,[Segment])
advanceWindow seg rw

    -- there were no other segments that might be unblocked by this one
  | null (rwSegments rw) =
    (moveRecvWindow (segNext seg) rw, [seg])

    -- see if this segment unblocks any others
  | otherwise =
    let (valid,rest) = splitContiguous (seg : rwSegments rw)
        -- XXX should this segment be checked for overlap with rwSegments?
     in (moveRecvWindow (segNext (last valid)) rw { rwSegments = rest }, valid)


-- | Insert a new segment into the receive window. NOTE: we don't need to worry
-- about trimming the segment to fit the window, as that's already been done by
-- sequenceNumberValid.
insertOutOfOrder :: Segment -> RecvWindow -> RecvWindow
insertOutOfOrder seg RecvWindow { .. } =
  RecvWindow { rwSegments = segs'
             , rwRcvWnd   = rwMax - fromIntegral (sum (map segLen segs'))
             , .. }
  where
  segs' = loop seg rwSegments

  loop new segs@(x:xs)

      -- new segment ends before x starts
    | segEnd new < segStart x = new : segs

      -- new segment starts after x
    | segStart new > segEnd x =
      x : loop new segs

      -- segments overlap
    | otherwise = resolveOverlap new x ++ xs

  loop new [] = [new]



-- | Split out contiguous segments, and out of order segments. NOTE: this
-- assumes that the segment list given does not contain any overlapping
-- segments, and is ordered.
--
-- INVARIANT: in the case that this is given a non-empty list, the list of valid
-- segments will always be non-null.
splitContiguous :: [Segment] -> ([Segment],[Segment])
splitContiguous []         = ([],[])
splitContiguous (seg:segs) = loop [seg] (segNext seg) segs
  where
  loop acc from (x:xs) | segStart x == from = loop (x:acc) (segNext seg) xs
  loop acc _    _                           = (reverse acc, [])


-- | Move the receive window to start at this new sequence number, and
-- recalculate the window size to accommodate the set of out-of-order segments.
moveRecvWindow :: TcpSeqNum -> RecvWindow -> RecvWindow
moveRecvWindow rcvNxt RecvWindow { .. } =
  RecvWindow { rwRcvNxt   = rcvNxt
             , rwRcvWnd   = rwMax - fromIntegral (sum (map segLen rwSegments))
             , rwSegments = trimSegments rcvNxt rwSegments
             , .. }


-- | Trim segments down to fit within the receive window.
trimSegments :: TcpSeqNum -> [Segment] -> [Segment]
trimSegments rcvNxt segs =
  case dropWhile (\seg -> segEnd seg < rcvNxt) segs of

    -- fit this segment to the front of the window
    seg : rest ->
      case trimSeg (fromTcpSeqNum (rcvNxt - segStart seg)) seg of
        Just seg' -> seg' : rest
        Nothing   -> rest

    []         -> []



-- Window Checks ---------------------------------------------------------------

-- | This is the check described on page 68 of RFC793, which checks that data
-- falls within the expected receive window. When the check is successful, the
-- segment returned is one that has been trimmed to fit in the window (if
-- necessary).
--
-- When this produces a segment, the segment has these properties:
--
--  1. The sequence number is within the window
--  2. The segment body falls within the window
--  3. The segment has been copied from the original bytestring
--
-- The reason for point 3 is that when frames are allocated by devices, they are
-- likely allocated to the full MTU, and not resized. Copying here frees up some
-- memory.
sequenceNumberValid :: TcpSeqNum  -- ^ RCV.NXT
                    -> TcpSeqNum  -- ^ RCV.WND
                    -> TcpHeader
                    -> S.ByteString
                    -> Maybe Segment

sequenceNumberValid rcvNxt rcvWnd hdr@TcpHeader { .. } payload

  | payloadLen == 0 =
    if rcvWnd == 0
       -- test 1
       then if tcpSeqNum == rcvNxt then Just (mkSegment hdr S.empty) else Nothing

       -- test 2
       else if seqNumInWindow      then Just (mkSegment hdr S.empty) else Nothing

  | otherwise =
    if rcvWnd == 0
       -- test 3
       then Nothing

       -- test 4
       else if | seqNumInWindow && dataEndInWindow -> Just (mkSegment hdr  seg')
               | seqNumInWindow                    -> Just (mkSegment hdr  seg')
               | dataEndInWindow                   -> Just (mkSegment hdr' seg')
               | otherwise                         -> Nothing

  where

  payloadLen = S.length payload

  segEnd = tcpSeqNum + fromIntegral payloadLen - 1

  hdr' = hdr { tcpSeqNum = rcvNxt }

  seg' = S.copy $ S.drop (fromTcpSeqNum (rcvNxt - tcpSeqNum))
                $ S.take (fromTcpSeqNum (segEnd - rcvWnd)) payload

  winEnd          = rcvNxt + rcvWnd
  seqNumInWindow  = rcvNxt <= tcpSeqNum && tcpSeqNum < winEnd
  dataEndInWindow = rcvNxt <= segEnd    && segEnd    < winEnd
