{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Layer.Tcp.Types where

import Hans.Address.IP4
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp

import Control.Exception (Exception,SomeException,toException)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Int (Int64)
import Data.Word (Word16)
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

  , tcpUserClosed  :: Bool
  , tcpMaxSegSize  :: !Int64
  , tcpOut         :: Window Outgoing
  , tcpOutBuffer   :: Buffer Outgoing
  , tcpInBuffer    :: Buffer Incoming

  , tcpNeedsDelAck :: Bool
  , tcpMaxIdle     :: !SlowTicks
  , tcpIdle        :: !SlowTicks

    -- retransmit timer
  , tcpRTO         :: !SlowTicks
  , tcpSRTT        :: !POSIXTime
  , tcpRTTVar      :: !POSIXTime

  , tcpTimer2MSL   :: !SlowTicks
  , tcpTimerRTO    :: !SlowTicks
  }

emptyTcpSocket :: Word16 -> TcpSocket
emptyTcpSocket sendWindow = TcpSocket
  { tcpParent      = Nothing
  , tcpSocketId    = emptySocketId
  , tcpState       = Closed
  , tcpAcceptors   = Seq.empty
  , tcpSndNxt      = 0
  , tcpSndUna      = 0
  , tcpRcvNxt      = 0

  , tcpUserClosed  = False
  , tcpMaxSegSize  = 1400
  , tcpOut         = emptyWindow sendWindow
  , tcpOutBuffer   = emptyBuffer 16384
  , tcpInBuffer    = emptyBuffer 16384

  , tcpNeedsDelAck = False
  , tcpMaxIdle     = 10 * 60 * 2
  , tcpIdle        = 0

  , tcpRTO         = 2
  , tcpSRTT        = 0
  , tcpRTTVar      = 0

  , tcpTimer2MSL   = 0
  , tcpTimerRTO    = 0
  }

nextSegSize :: TcpSocket -> Int64
nextSegSize tcp =
  min (fromIntegral (winAvailable (tcpOut tcp))) (tcpMaxSegSize tcp)

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
