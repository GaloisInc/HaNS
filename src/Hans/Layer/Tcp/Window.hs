{-# LANGUAGE RecordWildCards #-}

module Hans.Layer.Tcp.Window (
    module Hans.Layer.Tcp.Window
  , module Hans.Layer.Tcp.WaitBuffer
  ) where

import Hans.Layer.Tcp.WaitBuffer
import Hans.Message.Tcp

import Control.Monad (mzero)
import Data.Bits (shiftL)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word16,Word32)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T


-- Remote Window ---------------------------------------------------------------

-- | Remote window management.
data RemoteWindow = RemoteWindow
  { rwSegments     :: OutSegments
  , rwAvailable    :: !Word32
  , rwSize         :: !Word32
  , rwSndWind      :: !Word16
  , rwSndWindScale :: !Int
  } deriving (Show)

-- | The empty window, seeded with an initial size.
emptyRemoteWindow :: Word16 -> Int -> RemoteWindow
emptyRemoteWindow size scale = refreshRemoteWindow $! RemoteWindow
  { rwSegments     = Seq.empty
  , rwAvailable    = 0
  , rwSize         = 0
  , rwSndWind      = fromIntegral size
  , rwSndWindScale = scale
  }

-- | Recalculate internal constants of the remote window.
refreshRemoteWindow :: RemoteWindow -> RemoteWindow
refreshRemoteWindow rw = rw
  { rwSize      = size
  , rwAvailable = avail
  }
  where
  size                = fromIntegral (rwSndWind rw) `shiftL` rwSndWindScale rw
  used                = rwSize rw - rwAvailable rw
  avail | used > size = 0
        | otherwise   = size - used


-- | Set the Snd.Wind.Scale variable for the remote window.
setSndWindScale :: Int -> RemoteWindow -> RemoteWindow
setSndWindScale scale rw = refreshRemoteWindow $! rw
  { rwSndWindScale = scale
  }

-- | Set the Snd.Wind variable for the remote window.
setSndWind :: Word16 -> RemoteWindow -> RemoteWindow
setSndWind size rw = refreshRemoteWindow $! rw
  { rwSndWind = size
  }

-- | Adjust the internal available counter.
releaseSpace :: Word32 -> RemoteWindow -> RemoteWindow
releaseSpace len rw = rw
  { rwAvailable = min (rwSize rw) (rwAvailable rw + len)
  }

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

  -- XXX this doesn't deal with partial acks
  let match seg   = tcpSeqNum (outHeader seg) < tcpAckNum hdr
                 && outAckNum seg <= tcpAckNum hdr
      (acks,rest) = Seq.spanl match (rwSegments win)

  -- require that there was something to ack.
  case Seq.viewr acks of
    _ Seq.:> seg -> do
      let len  = F.sum (outSize `fmap` acks)
          win' = setSndWind (tcpWindow hdr)
               $ releaseSpace len
               $ win { rwSegments = rest }
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

clearRetransmit :: RemoteWindow -> RemoteWindow
clearRetransmit rw = rw { rwSegments = Seq.empty }

retransmitEmpty :: RemoteWindow -> Bool
retransmitEmpty rw = Seq.null (rwSegments rw)


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

mkOutSegment :: POSIXTime -> Int -> TcpHeader -> L.ByteString -> OutSegment
mkOutSegment created rto hdr body =
  OutSegment { outAckNum = tcpSeqNum hdr + outSize' hdr body
             , outTime   = created
             , outFresh  = True
             , outRTO    = rto
             , outHeader = hdr
             , outBody   = body
             }

ctlLength :: Num len => Bool -> len
ctlLength True  = 1
ctlLength False = 0

outSize :: Num a => OutSegment -> a
outSize OutSegment { .. } = outSize' outHeader outBody

-- | The size of a segment body.
outSize' :: Num a => TcpHeader -> L.ByteString -> a
outSize' TcpHeader { .. } body = fromIntegral (L.length body)
                               + ctlLength tcpSyn
                               + ctlLength tcpFin


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
-- sequence number, relative to RCV.NXT.
--
-- XXX make this respect the values of size and available
data LocalWindow = LocalWindow
  { lwBuffer       :: Seq.Seq InSegment
  , lwRcvNxt       :: !TcpSeqNum
  , lwAvailable    :: !Word32
  , lwSize         :: !Word32
  , lwRcvWind      :: !Word16
  , lwRcvWindScale :: !Int
  } deriving (Show)

-- | Empty local buffer, with an initial sequence number as the next expected
-- sequence number.
emptyLocalWindow :: TcpSeqNum -> Word16 -> Int -> LocalWindow
emptyLocalWindow sn size scale = refreshLocalWindow $! LocalWindow
  { lwBuffer       = Seq.empty
  , lwRcvNxt       = sn
  , lwAvailable    = 0
  , lwSize         = 0
  , lwRcvWind      = size
  , lwRcvWindScale = scale
  }

-- | Produce a sequence of blocks for the sack option.
localWindowSackBlocks :: LocalWindow -> Seq.Seq SackBlock
localWindowSackBlocks lw = case Seq.viewl (fmap mkSackBlock (lwBuffer lw)) of
  b Seq.:< rest -> uncurry (Seq.|>) (F.foldl step (Seq.empty,b) rest)
  Seq.EmptyL    -> Seq.empty
  where
  step (bs,b) b'
    | sbRight b == sbLeft b' = (bs,b { sbRight = sbRight b' })
    | otherwise              = (bs Seq.|> b, b')

-- | Recalculate internal constants.
refreshLocalWindow :: LocalWindow -> LocalWindow
refreshLocalWindow lw = lw
  { lwSize = fromIntegral (lwRcvWind lw) `shiftL` lwRcvWindScale lw
  }

-- | Update the size of the remote window.
setRcvNxt :: TcpSeqNum -> LocalWindow -> LocalWindow
setRcvNxt sn lw = lw { lwRcvNxt = sn }

-- | Add a sequence number to the value of Rcv.Nxt.
addRcvNxt :: TcpSeqNum -> LocalWindow -> LocalWindow
addRcvNxt sn win = win { lwRcvNxt = lwRcvNxt win + sn }

-- | Set the Rcv.Wind variable for the local window.
setRcvWind :: Word16 -> LocalWindow -> LocalWindow
setRcvWind size lw = refreshLocalWindow $! lw
  { lwRcvWind = size
  }

-- | Set the Rcv.Wind.Scale variable in the local window.
setRcvWindScale :: Int -> LocalWindow -> LocalWindow
setRcvWindScale scale lw = refreshLocalWindow $! lw
  { lwRcvWindScale = scale
  }

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

mkSackBlock :: InSegment -> SackBlock
mkSackBlock is = SackBlock
  { sbLeft  = sn
  , sbRight = sn + fromIntegral (S.length (inBody is))
  }
  where
  sn = tcpSeqNum (inHeader is)
