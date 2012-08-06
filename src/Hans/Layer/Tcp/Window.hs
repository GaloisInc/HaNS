{-# LANGUAGE EmptyDataDecls #-}

module Hans.Layer.Tcp.Window where

import Hans.Layer.Tcp.Types
import Hans.Message.Tcp

import Control.Monad (guard)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

data Incoming

-- | The outgoing window.
data Outgoing

-- | TCP windows, with a phantom type that determines the direction of packet
-- flow.
data Window d = Window
  { winSegments  :: Seq.Seq Segment
  , winAvailable :: !Word16
  , winSize      :: !Word16
  }

-- | The empty window, seeded with an initial size.
emptyWindow :: Word16 -> Window d
emptyWindow size = Window
  { winSegments  = Seq.empty
  , winAvailable = size
  , winSize      = size
  }

-- | Add a segment to the window.
addSegment :: Segment -> Window d -> Maybe (Window d)
addSegment seg win = do
  let size = segSize seg
  guard (winAvailable win >= segSize seg)
  return win
    { winSegments  = winSegments win Seq.|> seg
    , winAvailable = winAvailable win - size
    }

-- | Process an incoming ack, returning a finalizer, and a new window if there
-- was a matching set of packets waiting for an ack.
receiveAck :: TcpHeader -> Window Outgoing
           -> Maybe (Finalizer, Window Outgoing)
receiveAck hdr win = do
  let match seg = segSeqNum seg == tcpSeqNum hdr
      (acks,rest) = Seq.breakl match (winSegments win)
  guard (not (Seq.null acks)) -- something got ack'd
  let (fin,len) = F.foldl finalize (return (),0) acks
      win' = win
        { winSegments  = rest
        , winAvailable = winAvailable win + fromIntegral len
        }
  return (fin, win')
  where
  finalize (fin,len) seg = (fin',len')
    where
    fin' = maybe fin (fin >>) (segFinalizer seg)
    len' = len + L.length (segBody seg)
