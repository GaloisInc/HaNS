{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Hans.Tcp.RecvWindow (
    -- * Receive Window
    Window(),
    emptyWindow,
    recvSegment,
    rcvWnd, rcvNxt, setRcvNxt,

    -- ** Segments
    Segment(..),

    -- ** Sequence Numbers
    sequenceNumberValid,
  ) where

import           Hans.Lens
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
          , segEnd   = tcpSegLastSeqNum segHdr (S.length segBody)
          , .. }

-- | The next segment number, directly after this one.
segNext :: Segment -> TcpSeqNum
segNext Segment { .. } = segEnd + 1

segLen :: Segment -> Int
segLen Segment { .. } = tcpSegLen segHdr (S.length segBody)

-- | Drop data off of the front of this segment.
trimSeg :: Int -> Segment -> Maybe Segment
trimSeg len seg@Segment { .. }
  | len' <= 0 =
    Just seg

  | len' >= S.length segBody =
    Nothing

  | otherwise =
    Just $! Segment { segStart = segStart + fromIntegral len'
                    , segHdr   = segHdr { tcpSeqNum = tcpSeqNum segHdr
                                                    + fromIntegral len }
                    , segBody  = S.drop len segBody
                    , .. }

  where

  flag l | view l segHdr = 1
         | otherwise     = 0

  -- adjust the length to account for syn/fin
  len' = len - flag tcpSyn - flag tcpFin

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
data Window = Window { wSegments :: ![Segment]
                       -- ^ Out of order segments.

                     , wRcvNxt :: !TcpSeqNum
                       -- ^ Left-edge of the receive window

                     , wRcvWnd :: !TcpSeqNum

                       -- ^ Current size of the receive window

                     , wMax :: !TcpSeqNum
                       -- ^ Maximum size of the receive window

                     } deriving (Show)

emptyWindow :: TcpSeqNum -> Int -> Window
emptyWindow wRcvNxt maxWin =
  Window { wSegments = []
         , wRcvWnd   = wRcvNxt + wMax
         , .. }
  where
  wMax = fromIntegral maxWin

rcvWnd :: Getting r Window TcpSeqNum
rcvWnd  = to wRcvWnd

rcvNxt :: Getting r Window TcpSeqNum
rcvNxt  = to wRcvNxt

-- | Only sets RCV.NXT when the segment queue is empty. Returns 'True' when the
-- value has been successfully changed.
setRcvNxt :: TcpSeqNum -> Window -> (Window,Bool)
setRcvNxt nxt win
  | null (wSegments win) = (win { wRcvNxt = nxt }, True)
  | otherwise            = (win, False)

-- | Check an incoming segment, and queue it in the receive window. The boolean
-- parameter on the second element of the pair is True when the segment received
-- was valid.
recvSegment :: TcpHeader -> S.ByteString -> Window
            -> (Window, (Bool,[Segment]))

recvSegment hdr body win

  | Just seg <- sequenceNumberValid (wRcvNxt win) (wRcvWnd win) hdr body =
    let (win',segs) = addSegment seg win
     in (win',(True,segs))

    -- drop the invalid frame
  | otherwise =
    (win, (False,[]))


-- | Add a validated segment to the receive window, and return 
addSegment :: Segment -> Window -> (Window,[Segment])
addSegment seg win

    -- The new segment falls right at the beginning of the receive window
  | segStart seg == wRcvNxt win =
    advanceWindow seg win

    -- As addSegment should only be called with the results of
    -- sequenceNumberValid, the only remaining case to consider is that the
    -- segment falls somewhere else within the window.
  | otherwise =
    (insertOutOfOrder seg win, [])


-- | Use this segment to advance the window, which may unblock zero or more out
-- of order segments. The list returned is always non-empty, as it includes the
-- segment that's given.
advanceWindow :: Segment -> Window -> (Window,[Segment])
advanceWindow seg win

    -- there were no other segments that might be unblocked by this one
  | null (wSegments win) =
    (moveWindow (segNext seg) win, [seg])

    -- see if this segment unblocks any others
  | otherwise =
    let (valid,rest) = splitContiguous (seg : wSegments win)
        -- XXX should this segment be checked for overlap with rwSegments?
     in (moveWindow (segNext (last valid)) win { wSegments = rest }, valid)


-- | Insert a new segment into the receive window. NOTE: we don't need to worry
-- about trimming the segment to fit the window, as that's already been done by
-- sequenceNumberValid.
insertOutOfOrder :: Segment -> Window -> Window
insertOutOfOrder seg Window { .. } =
  Window { wSegments = segs'
         , wRcvWnd   = wMax - fromIntegral (sum (map segLen segs'))
         , .. }
  where
  segs' = loop seg wSegments

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
moveWindow :: TcpSeqNum -> Window -> Window
moveWindow nxt Window { .. } =
  Window { wRcvNxt   = nxt
         , wRcvWnd   = wMax - fromIntegral (sum (map segLen wSegments))
         , wSegments = trimSegments nxt wSegments
         , .. }


-- | Trim segments down to fit within the receive window.
trimSegments :: TcpSeqNum -> [Segment] -> [Segment]
trimSegments nxt segs =
  case dropWhile (\seg -> segEnd seg < nxt) segs of

    -- fit this segment to the front of the window
    seg : rest ->
      case trimSeg (fromTcpSeqNum (nxt - segStart seg)) seg of
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

sequenceNumberValid nxt wnd hdr@TcpHeader { .. } payload

  | payloadLen == 0 =
    if wnd == 0
       -- test 1
       then if tcpSeqNum == nxt then Just (mkSegment hdr S.empty) else Nothing

       -- test 2
       else if seqNumInWindow   then Just (mkSegment hdr S.empty) else Nothing

  | otherwise =
    if wnd == 0
       -- test 3
       then Nothing

       -- test 4
       else if | seqNumInWindow && dataEndInWindow -> Just (mkSegment hdr  seg')
               | seqNumInWindow                    -> Just (mkSegment hdr  seg')
               | dataEndInWindow                   -> Just (mkSegment hdr' seg')
               | otherwise                         -> Nothing

  where

  payloadLen = tcpSegLen hdr (S.length payload)

  segEnd = tcpSeqNum + fromIntegral payloadLen - 1

  hdr' = hdr { tcpSeqNum = nxt }

  seg' = S.copy $ S.drop (fromTcpSeqNum (nxt    - tcpSeqNum))
                $ S.take (fromTcpSeqNum (segEnd - wnd)) payload

  winEnd          = nxt + wnd
  seqNumInWindow  = nxt <= tcpSeqNum && tcpSeqNum < winEnd
  dataEndInWindow = nxt <= segEnd    && segEnd    < winEnd
