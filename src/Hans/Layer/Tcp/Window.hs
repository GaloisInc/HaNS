{-# LANGUAGE EmptyDataDecls #-}

module Hans.Layer.Tcp.Window where

import Hans.Message.Tcp

import Control.Monad (guard)
import Data.Int (Int64)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


-- | Incoming data window.
data Incoming

-- | Outgoing data window.
data Outgoing

-- | TCP windows, with a phantom type that determines the direction of packet
-- flow.
data Window d = Window
  { winSegments  :: Seq.Seq Segment
  , winAvailable :: !Word16
  , winSize      :: !Word16
  } deriving (Show)

-- | The empty window, seeded with an initial size.
emptyWindow :: Word16 -> Window d
emptyWindow size = Window
  { winSegments  = Seq.empty
  , winAvailable = size
  , winSize      = size
  }

-- | Add a segment to the window.
addSegment :: Segment -> Window d -> Window d
addSegment seg win = win
  { winSegments  = winSegments win Seq.|> seg
  , winAvailable = winAvailable win - segSize seg
  }

-- | Process an incoming ack, returning a finalizer, and a new window if there
-- was a matching set of packets waiting for an ack.
receiveAck :: TcpHeader -> Window Outgoing
           -> Maybe (Window Outgoing)
receiveAck hdr win = do
  let match seg = segAckNum seg == tcpAckNum hdr
      (acks,rest) = Seq.spanl match (winSegments win)
  guard (not (Seq.null acks)) -- something got ack'd
  let len  = F.foldl (\l seg -> l + segSize seg) 0 acks
      win' = win
        { winSegments  = rest
        , winAvailable = winAvailable win + len
        }
  return win'


-- Segments --------------------------------------------------------------------

-- | A delivered segment.
data Segment = Segment
  { segAckNum :: !TcpSeqNum
  , segHeader :: !TcpHeader
  , segBody   :: !L.ByteString
  } deriving (Show)

-- | The size of a segment body.
segSize :: Num a => Segment -> a
segSize  = fromIntegral . L.length . segBody


-- Chunk Buffering -------------------------------------------------------------

-- | The boolean parameter indicates whether or not the continuation is being
-- called in the event of a shutdown, or if the computation should be re-run,
-- allowing for more data to flow.
type Wakeup = Bool -> IO ()

-- | Indicate that the action should be retried.
tryAgain :: Wakeup -> IO ()
tryAgain f = f True

-- | Indicate that the action should not be retried.
abort :: Wakeup -> IO ()
abort f = f False


data Buffer d = Buffer
  { bufBytes   :: L.ByteString
  , bufWaiting :: Seq.Seq Wakeup
  , bufSize    :: !Int64
  }

-- | An empty buffer, with a limit.
emptyBuffer :: Int64 -> Buffer d
emptyBuffer size = Buffer
  { bufBytes   = L.empty
  , bufWaiting = Seq.empty
  , bufSize    = size
  }

-- | Queue a wakeup action into a buffer.
queueWaiting :: Wakeup -> Buffer d -> Buffer d
queueWaiting wakeup buf = buf { bufWaiting = bufWaiting buf Seq.|> wakeup }

-- | Take a single Wakeup action off of a buffer.
takeWaiting :: Buffer d -> Maybe (Wakeup,Buffer d)
takeWaiting buf = case Seq.viewl (bufWaiting buf) of
  w Seq.:< ws -> Just (w,buf { bufWaiting = ws })
  Seq.EmptyL  -> Nothing

-- | Queue bytes into a buffer that has some available size.
queueBytes :: L.ByteString -> Buffer d -> (Maybe Int64, Buffer d)
queueBytes bytes buf
  | avail <= 0 = (Nothing,buf)
  | otherwise  = (Just (L.length queued), buf')
  where
  used   = L.length (bufBytes buf)
  avail  = bufSize buf - used
  queued = L.take avail bytes
  buf'   = buf { bufBytes = bufBytes buf `L.append` queued }

-- | Run all waiting continuations with a parameter of False, 
shutdownWaiting :: Buffer d -> (IO (), Buffer d)
shutdownWaiting buf = (m,buf { bufWaiting = Seq.empty })
  where
  m = F.mapM_ abort (bufWaiting buf)


-- Sending Buffer --------------------------------------------------------------

-- | Queue bytes in an outgoing buffer.  When the number of bytes written is
-- @Nothing@, the wakeup action has been queued.
writeBytes :: L.ByteString -> Wakeup -> Buffer Outgoing
           -> (Maybe Int64,Buffer Outgoing)
writeBytes bytes wakeup buf = case queueBytes bytes buf of
  (Nothing,buf') -> (Nothing,queueWaiting wakeup buf')
  res            -> res

-- | Test the size of the output buffer.
bytesAvailable :: Buffer Outgoing -> Bool
bytesAvailable  = not . L.null . bufBytes

-- | Take bytes off of a sending queue, making room new data.
takeBytes :: Int64 -> Buffer Outgoing
          -> Maybe (Maybe Wakeup,L.ByteString,Buffer Outgoing)
takeBytes len buf = do
  guard (bytesAvailable buf)
  let (bytes,rest) = L.splitAt len (bufBytes buf)
      buf'         = buf { bufBytes = rest }
  case Seq.viewl (bufWaiting buf) of
    Seq.EmptyL  -> return (Nothing, bytes, buf')
    w Seq.:< ws -> return (Just w, bytes, buf' { bufWaiting = ws })


-- Receiving Buffer ------------------------------------------------------------

-- | Read bytes from an incoming buffer, queueing if there are no bytes to read.
readBytes :: Int64 -> Wakeup -> Buffer Incoming
          -> (Maybe L.ByteString, Buffer Incoming)
readBytes len wakeup buf
  | L.null (bufBytes buf) = (Nothing, queueWaiting wakeup buf)
  | otherwise             = (Just bytes, buf { bufBytes = rest })
  where
  (bytes,rest) = L.splitAt len (bufBytes buf)

-- | Place bytes on the incoming buffer, provided that there is enough space for
-- all of the bytes.
putBytes :: L.ByteString -> Buffer Incoming
         -> Maybe (Maybe Wakeup,Buffer Incoming)
putBytes bytes buf = do
  let needed = L.length bytes + L.length (bufBytes buf)
  guard (needed < bufSize buf)
  let buf' = buf { bufBytes = bufBytes buf `L.append` bytes }
  case Seq.viewl (bufWaiting buf) of
    Seq.EmptyL  -> return (Nothing, buf')
    w Seq.:< ws -> return (Just w, buf' { bufWaiting = ws })
