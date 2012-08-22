{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Layer.Tcp.Window (
    module Hans.Layer.Tcp.Window
  , module Hans.Layer.Tcp.WaitBuffer
  ) where

import Hans.Layer.Tcp.WaitBuffer
import Hans.Message.Tcp

import Control.Monad (mzero)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word16)
import qualified Data.ByteString as S
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
      let len  = F.sum (segSize `fmap` acks)
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


-- Local Window ----------------------------------------------------------------

-- | Local window, containing a buffer of incoming packets, indexed by their 
data LocalWindow = LocalWindow
  { lwBuffer :: Seq.Seq InSegment
  , lwRcvNxt :: !TcpSeqNum
  } deriving (Show)

-- | Empty local buffer, with an initial sequence number as the next expected
-- sequence number.
emptyLocalWindow :: TcpSeqNum -> LocalWindow
emptyLocalWindow sn = LocalWindow
  { lwBuffer = Seq.empty
  , lwRcvNxt = sn
  }

setRcvNxt :: TcpSeqNum -> LocalWindow -> LocalWindow
setRcvNxt sn win = win { lwRcvNxt = sn }

addRcvNxt :: TcpSeqNum -> LocalWindow -> LocalWindow
addRcvNxt sn win = win { lwRcvNxt = lwRcvNxt win + sn }

-- | Process an incoming packet that needs to pass through the incoming queue.
incomingPacket :: TcpHeader -> S.ByteString -> LocalWindow
               -> (Seq.Seq InSegment, LocalWindow)
incomingPacket hdr body win = stepWindow (addInSegment hdr body win)

-- | Queue an incoming packet in the incoming window.
addInSegment :: TcpHeader -> S.ByteString -> LocalWindow -> LocalWindow
addInSegment hdr body win = win { lwBuffer = insert (lwBuffer win) }
  where
  seg        = mkInSegment (lwRcvNxt win) hdr body
  insert buf = case Seq.viewl buf of
    a Seq.:< rest -> case compare (isRelSeqNum a) (isRelSeqNum seg) of
      LT -> a   Seq.<| insert rest
      EQ -> seg Seq.<| rest
      GT -> seg Seq.<| buf
    Seq.EmptyL    -> Seq.singleton seg

-- | Advance the window, if there are packets available to be returned.
stepWindow :: LocalWindow -> (Seq.Seq InSegment, LocalWindow)
stepWindow  = loop 0 Seq.empty
  where
  loop expect segs win = case Seq.viewl (lwBuffer win) of
    a Seq.:< rest | isRelSeqNum a == expect -> keep a segs rest win
                  | otherwise               -> finalize segs win
    Seq.EmptyL                              -> finalize segs win

  keep seg segs rest win = loop (isRelSeqNum seg + len) (segs Seq.|> seg) win
    { lwBuffer = rest
    , lwRcvNxt = lwRcvNxt win + len
    }
    where
    len = fromIntegral (S.length (isBody seg))

  finalize segs win = (segs,win { lwBuffer = update `fmap` lwBuffer win })
    where
    len       = F.sum (isLength `fmap` segs)
    update is = is { isRelSeqNum = isRelSeqNum is - len }

data InSegment = InSegment
  { isRelSeqNum :: !TcpSeqNum
  , isHeader    :: !TcpHeader
  , isBody      :: !S.ByteString
  } deriving (Show)

isSeqNum :: InSegment -> TcpSeqNum
isSeqNum  = tcpSeqNum . isHeader

isLength :: Num a => InSegment -> a
isLength  = fromIntegral . S.length . isBody

-- | Generate an incoming segment, relative to the value of RCV.NXT
mkInSegment :: TcpSeqNum -> TcpHeader -> S.ByteString -> InSegment
mkInSegment rcvNxt hdr body = InSegment
  { isRelSeqNum = rel
  , isHeader    = hdr
  , isBody      = body
  }
  where
  -- calculate the index, relative to RCV.NXT, taking sequence number wrap into
  -- account
  rel | tcpSeqNum hdr < rcvNxt = maxBound      - rcvNxt + tcpSeqNum hdr + 1
      | otherwise              = tcpSeqNum hdr - rcvNxt
