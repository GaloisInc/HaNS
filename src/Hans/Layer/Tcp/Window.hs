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

-- | Remote window management.
data RemoteWindow = RemoteWindow
  { rwSegments  :: OutSegments
  , rwAvailable :: !Word16
  , rwSize      :: !Word16
  } deriving (Show)

-- | The empty window, seeded with an initial size.
emptyRemoteWindow :: Word16 -> RemoteWindow
emptyRemoteWindow size = RemoteWindow
  { rwSegments  = Seq.empty
  , rwAvailable = size
  , rwSize      = size
  }

resizeWindow :: Word16 -> RemoteWindow -> RemoteWindow
resizeWindow size win = win
  { rwSize      = size
  , rwAvailable = avail
  }
  where
  used                = rwSize win - rwAvailable win
  avail | used > size = 0
        | otherwise   = size - used

-- | Add a segment to the window.
addSegment :: OutSegment -> RemoteWindow -> RemoteWindow
addSegment seg win = win
  { rwSegments  = rwSegments win Seq.|> seg
  , rwAvailable = rwAvailable win - outSize seg
  }

-- | Process an incoming ack, returning a finalizer, and a new window if there
-- was a matching set of packets waiting for an ack.
receiveAck :: TcpHeader -> RemoteWindow -> Maybe (OutSegment,RemoteWindow)
receiveAck hdr win = do
  let match seg   = outAckNum seg == tcpAckNum hdr
      (acks,rest) = Seq.spanl match (rwSegments win)

  -- require that there was something to ack.
  case Seq.viewr acks of
    _ Seq.:> seg -> do
      let len  = F.sum (outSize `fmap` acks)
          win' = win
            { rwSegments  = rest
            , rwSize      = tcpWindow hdr
            , rwAvailable = min (tcpWindow hdr) (rwAvailable win + len)
            }
      return (seg, win')
    Seq.EmptyR -> mzero

-- | Update the RTO timer on all segments waiting for an ack.  When the timer
-- runs out, output the segment for retransmission.
genRetransmitSegments :: RemoteWindow -> (OutSegments,RemoteWindow)
genRetransmitSegments win = (outSegs, win { rwSegments = segs' })
  where
  (outSegs,segs') = T.mapAccumL step Seq.empty (rwSegments win)
  step rts seg = (rts',seg')
    where
    seg'                    = decrementRTO seg
    rts' | outRTO seg' <= 0 = rts Seq.|> seg'
         | otherwise        = rts


type OutSegments = Seq.Seq OutSegment

-- | A delivered segment.
data OutSegment = OutSegment
  { outAckNum :: !TcpSeqNum
  , outTime   :: !POSIXTime
  , outFresh  :: Bool       -- ^ Whether or not this is a retransmission
  , outRTO    :: !Int       -- ^ Retransmit timer for this segment
  , outHeader :: !TcpHeader
  , outBody   :: !L.ByteString
  } deriving (Show)

-- | The size of a segment body.
outSize :: Num a => OutSegment -> a
outSize  = fromIntegral . L.length . outBody

-- | Decrement the RTO value on a segment by the timer granularity (500ms).
-- Once the RTO value dips below 1, mark the segment as no longer fresh.
decrementRTO :: OutSegment -> OutSegment
decrementRTO seg
  | outRTO seg' <= 0 = seg' { outFresh = False }
  | otherwise        = seg'
  where
  seg' = seg { outRTO = outRTO seg - 1 }


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
    a Seq.:< rest -> case compare (inRelSeqNum a) (inRelSeqNum seg) of
      LT -> a   Seq.<| insert rest
      EQ -> seg Seq.<| rest
      GT -> seg Seq.<| buf
    Seq.EmptyL    -> Seq.singleton seg

-- | Advance the window, if there are packets available to be returned.
stepWindow :: LocalWindow -> (Seq.Seq InSegment, LocalWindow)
stepWindow  = loop 0 Seq.empty
  where
  loop expect segs win = case Seq.viewl (lwBuffer win) of
    a Seq.:< rest | inRelSeqNum a == expect -> keep a segs rest win
                  | otherwise               -> finalize segs win
    Seq.EmptyL                              -> finalize segs win

  keep seg segs rest win = loop (inRelSeqNum seg + len) (segs Seq.|> seg) win
    { lwBuffer = rest
    , lwRcvNxt = lwRcvNxt win + len
    }
    where
    len = fromIntegral (S.length (inBody seg))

  finalize segs win = (segs,win { lwBuffer = update `fmap` lwBuffer win })
    where
    len       = F.sum (inLength `fmap` segs)
    update is = is { inRelSeqNum = inRelSeqNum is - len }

data InSegment = InSegment
  { inRelSeqNum :: !TcpSeqNum
  , inHeader    :: !TcpHeader
  , inBody      :: !S.ByteString
  } deriving (Show)

inSeqNum :: InSegment -> TcpSeqNum
inSeqNum  = tcpSeqNum . inHeader

inLength :: Num a => InSegment -> a
inLength  = fromIntegral . S.length . inBody

-- | Generate an incoming segment, relative to the value of RCV.NXT
mkInSegment :: TcpSeqNum -> TcpHeader -> S.ByteString -> InSegment
mkInSegment rcvNxt hdr body = InSegment
  { inRelSeqNum = rel
  , inHeader    = hdr
  , inBody      = body
  }
  where
  -- calculate the index, relative to RCV.NXT, taking sequence number wrap into
  -- account
  rel | tcpSeqNum hdr < rcvNxt = maxBound      - rcvNxt + tcpSeqNum hdr + 1
      | otherwise              = tcpSeqNum hdr - rcvNxt
