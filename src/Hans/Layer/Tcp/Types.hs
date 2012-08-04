{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Layer.Tcp.Types where

import Hans.Address.IP4
import Hans.Message.Tcp

import Control.Exception (Exception,SomeException,toException)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Sequence as Seq


-- Hosts Information -----------------------------------------------------------

data Host = Host
  { hostConnections   :: Connections
  , hostInitialSeqNum :: !TcpSeqNum
  }

emptyHost :: Host
emptyHost  = Host
  { hostConnections   = Map.empty
    -- XXX what should we seed this with?
  , hostInitialSeqNum = 0
  }


-- Connections -----------------------------------------------------------------

type Connections = Map.Map SocketId TcpSocket

removeClosed :: Connections -> Connections
removeClosed  =
  Map.filter (\tcp -> tcpState tcp /= Closed || not (tcpUserClosed tcp))

data SocketId = SocketId
  { sidLocalPort  :: !TcpPort
  , sidRemotePort :: !TcpPort
  , sidRemoteHost :: !IP4
  } deriving (Eq,Show,Ord)

emptySocketId :: SocketId
emptySocketId  = SocketId
  { sidLocalPort  = TcpPort 0
  , sidRemotePort = TcpPort 0
  , sidRemoteHost = IP4 0 0 0 0
  }

listenSocketId :: TcpPort -> SocketId
listenSocketId port = emptySocketId { sidLocalPort = port }

incomingSocketId :: IP4 -> TcpHeader -> SocketId
incomingSocketId remote hdr = SocketId
  { sidLocalPort  = tcpDestPort hdr
  , sidRemotePort = tcpSourcePort hdr
  , sidRemoteHost = remote
  }

data SocketResult a
  = SocketResult a
  | SocketError SomeException
    deriving (Show)

socketError :: Exception e => e -> SocketResult a
socketError  = SocketError . toException

type Acceptor = SocketId -> IO ()

type Close = IO ()

type SlowTicks = Int

data TcpSocket = TcpSocket
  { tcpParent      :: Maybe SocketId
  , tcpSocketId    :: !SocketId
  , tcpState       :: !ConnState
  , tcpAcceptors   :: Seq.Seq Acceptor
  , tcpSndNxt      :: !TcpSeqNum
  , tcpSndUna      :: !TcpSeqNum
  , tcpRcvNxt      :: !TcpSeqNum
  , tcpSockWin     :: !Word16

  , tcpUserClosed  :: Bool
  , tcpOut         :: Output

  , tcpNeedsDelAck :: Bool
  , tcpMaxIdle     :: !SlowTicks
  , tcpIdle        :: !SlowTicks

  , tcpTimer2MSL   :: !SlowTicks
  }

emptyTcpSocket :: TcpSocket
emptyTcpSocket  = TcpSocket
  { tcpParent      = Nothing
  , tcpSocketId    = emptySocketId
  , tcpState       = Closed
  , tcpAcceptors   = Seq.empty
  , tcpSndNxt      = 0
  , tcpSndUna      = 0
  , tcpRcvNxt      = 0
  , tcpSockWin     = 0

  , tcpUserClosed  = False
  , tcpOut         = emptyOutput

  , tcpNeedsDelAck = False
  , tcpMaxIdle     = 10 * 60 * 2
  , tcpIdle        = 0

  , tcpTimer2MSL   = 0
  }

isAccepting :: TcpSocket -> Bool
isAccepting  = not . Seq.null . tcpAcceptors

data ConnState
  = Closed
  | Listen
  | SynSent
  | SynReceived
  | Established
  | CloseWait
  | FinWait1
  | FinWait2
  | Closing
  | LastAck
  | TimeWait
    deriving (Show,Eq,Ord)


-- Output ACK Management -------------------------------------------------------

type Output = Map.Map TcpSeqNum Segment

emptyOutput :: Output
emptyOutput  = Map.empty

waitForAck :: Segment -> Output -> Output
waitForAck seg = Map.insert ix seg
  where
  -- the acked seq num
  ix = tcpSeqNum (segHeader seg) + fromIntegral (L.length (segBody seg))

-- | Clear out a packet waiting for an ack.
registerAck :: TcpHeader -> Output -> (Maybe Finalizer,Output)
registerAck hdr out = (segFinalizer =<< mb, out')
  where
  (mb,out')  = Map.updateLookupWithKey remove (tcpAckNum hdr) out
  remove _ _ = Nothing

-- | Remove all finalizers from the Output queue.
removeFinalizers :: Output -> ([Finalizer],Output)
removeFinalizers  = Map.mapAccum step []
  where
  step fs seg = case segFinalizer seg of
    Just f  -> (f:fs, seg { segFinalizer = Nothing })
    Nothing -> (fs, seg)

type Finalizer = IO ()

-- | A delivered segment.
data Segment = Segment
  { segHeader    :: !TcpHeader
  , segBody      :: !L.ByteString
  , segFinalizer :: Maybe Finalizer
  }

segSize :: Num a => Segment -> a
segSize  = fromIntegral . L.length . segBody
