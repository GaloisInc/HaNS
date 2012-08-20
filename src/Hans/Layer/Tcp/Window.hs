module Hans.Layer.Tcp.Window (
    module Hans.Layer.Tcp.Window
  , module Hans.Layer.Tcp.WaitBuffer
  ) where

import Hans.Layer.Tcp.WaitBuffer
import Hans.Message.Tcp

import Control.Monad (mzero)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Monoid (Sum(..))
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T


-- Remote Window ---------------------------------------------------------------

-- | TCP windows, with a phantom type that determines the direction of packet
-- flow.
data Window = Window
  { winSegments  :: Segments
  , winAvailable :: !Word16
  , winSize      :: !Word16
  } deriving (Show)

-- | The empty window, seeded with an initial size.
emptyWindow :: Word16 -> Window
emptyWindow size = Window
  { winSegments  = Seq.empty
  , winAvailable = size
  , winSize      = size
  }

resizeWindow :: Word16 -> Window -> Window
resizeWindow size win = win
  { winSize      = size
  , winAvailable = avail
  }
  where
  used                = winSize win - winAvailable win
  avail | used > size = 0
        | otherwise   = size - used

-- | Add a segment to the window.
addSegment :: Segment -> Window -> Window
addSegment seg win = win
  { winSegments  = winSegments win Seq.|> seg
  , winAvailable = winAvailable win - segSize seg
  }

-- | Process an incoming ack, returning a finalizer, and a new window if there
-- was a matching set of packets waiting for an ack.
receiveAck :: TcpHeader -> Window -> Maybe (Segment,Window)
receiveAck hdr win = do
  let match seg = segAckNum seg == tcpAckNum hdr
      (acks,rest) = Seq.spanl match (winSegments win)

  -- require that there was something to ack.
  case Seq.viewr acks of
    _ Seq.:> seg -> do
      let len  = getSum (F.foldMap (Sum . segSize) acks)
          win' = win
            { winSegments  = rest
            , winSize      = tcpWindow hdr
            , winAvailable = min (tcpWindow hdr) (winAvailable win + len)
            }
      return (seg, win')
    Seq.EmptyR -> mzero

-- | Update the RTO timer on all segments waiting for an ack.  When the timer
-- runs out, output the segment for retransmission.
genRetransmitSegments :: Window -> (Segments,Window)
genRetransmitSegments win = (outSegs, win { winSegments = segs' })
  where
  (outSegs,segs') = T.mapAccumL step Seq.empty (winSegments win)
  step rts seg = (rts',seg')
    where
    seg'                    = decrementRTO seg
    rts' | segRTO seg' <= 0 = rts Seq.|> seg'
         | otherwise        = rts


-- Segments --------------------------------------------------------------------

type Segments = Seq.Seq Segment

-- | A delivered segment.
data Segment = Segment
  { segAckNum :: !TcpSeqNum
  , segTime   :: !POSIXTime
  , segFresh  :: Bool       -- ^ Whether or not this is a retransmission
  , segRTO    :: !Int       -- ^ Retransmit timer for this segment
  , segHeader :: !TcpHeader
  , segBody   :: !L.ByteString
  } deriving (Show)

-- | The size of a segment body.
segSize :: Num a => Segment -> a
segSize  = fromIntegral . L.length . segBody

-- | Decrement the RTO value on a segment by the timer granularity (500ms).
-- Once the RTO value dips below 1, mark the segment as no longer fresh.
decrementRTO :: Segment -> Segment
decrementRTO seg
  | segRTO seg' <= 0 = seg' { segFresh = False }
  | otherwise        = seg'
  where
  seg' = seg { segRTO = segRTO seg - 1 }
