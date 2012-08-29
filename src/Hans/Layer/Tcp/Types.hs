{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Layer.Tcp.Types where

import Hans.Address.IP4
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp
import Hans.Ports

import Control.Exception (Exception,SomeException,toException)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word16,Word32)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq


-- Hosts Information -----------------------------------------------------------

data Host = Host
  { hostConnections   :: Connections
  , hostInitialSeqNum :: !TcpSeqNum
  , hostPorts         :: !(PortManager TcpPort)
  }

emptyHost :: Host
emptyHost  = Host
  { hostConnections   = Map.empty
    -- XXX what should we seed this with?
  , hostInitialSeqNum = 0
  , hostPorts         = emptyPortManager [32768 .. 61000]
  }

takePort :: Host -> Maybe (TcpPort,Host)
takePort host = do
  (p,ps) <- nextPort (hostPorts host)
  return (p, host { hostPorts = ps })

releasePort :: TcpPort -> Host -> Host
releasePort p host = fromMaybe host $ do
  ps <- unreserve p (hostPorts host)
  return host { hostPorts = ps }


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


-- Timers ----------------------------------------------------------------------

type SlowTicks = Int

data TcpTimers = TcpTimers
  { ttDelayedAck :: !Bool

    -- 2MSL
  , tt2MSL       :: !SlowTicks

    -- retransmit timer
  , ttRTO        :: !SlowTicks
  , ttSRTT       :: !POSIXTime
  , ttRTTVar     :: !POSIXTime

    -- idle timer
  , ttMaxIdle    :: !SlowTicks
  , ttIdle       :: !SlowTicks
  }

emptyTcpTimers :: TcpTimers
emptyTcpTimers  = TcpTimers
  { ttDelayedAck = False
  , tt2MSL       = 0
  , ttRTO        = 2 -- one second
  , ttSRTT       = 0
  , ttRTTVar     = 0
  , ttMaxIdle    = 10 * 60 * 2
  , ttIdle       = 0
  }


-- Timestamp Management --------------------------------------------------------

-- | Manage the timestamp values that are in flight between two hosts.
data Timestamp = Timestamp
  { tsTimestamp     :: !Word32
  , tsLastTimestamp :: !Word32
  , tsGranularity   :: !POSIXTime -- ^ Hz
  , tsLastUpdate    :: !POSIXTime
  } deriving (Show)

emptyTimestamp :: POSIXTime -> Timestamp
emptyTimestamp start = Timestamp
  { tsTimestamp     = 0
  , tsLastTimestamp = 0
  , tsGranularity   = 1000
  , tsLastUpdate    = start
  }

-- | Update the timestamp value, advancing based on the timestamp granularity.
-- If the number of ticks to advance is 0, don't advance the timestamp.
stepTimestamp :: POSIXTime -> Timestamp -> Timestamp
stepTimestamp now ts
  | diff == 0 = ts
  | otherwise = ts
    { tsLastUpdate = now
    , tsTimestamp  = tsTimestamp ts + diff
    }
  where
  diff = ceiling (tsGranularity ts * (now - tsLastUpdate ts))

-- | Generate timestamp option for an outgoing packet.
mkTimestamp :: Timestamp -> TcpOption
mkTimestamp ts = OptTimestamp (tsTimestamp ts) (tsLastTimestamp ts)


-- Internal Sockets ------------------------------------------------------------

type Acceptor = SocketId -> IO ()

type Notify = Bool -> IO ()

type Close = IO ()

data TcpSocket = TcpSocket
  { tcpParent     :: Maybe SocketId
  , tcpSocketId   :: !SocketId
  , tcpState      :: !ConnState
  , tcpAcceptors  :: Seq.Seq Acceptor
  , tcpNotify     :: Maybe Notify
  , tcpSndNxt     :: !TcpSeqNum
  , tcpSndUna     :: !TcpSeqNum

  , tcpUserClosed :: Bool
  , tcpOut        :: RemoteWindow
  , tcpOutBuffer  :: Buffer Outgoing
  , tcpOutMSS     :: !Int64
  , tcpIn         :: LocalWindow
  , tcpInBuffer   :: Buffer Incoming
  , tcpInMSS      :: !Int64

  , tcpTimers     :: !TcpTimers
  , tcpTimestamp  :: Maybe Timestamp
  }

emptyTcpSocket :: Word16 -> TcpSocket
emptyTcpSocket sendWindow = TcpSocket
  { tcpParent     = Nothing
  , tcpSocketId   = emptySocketId
  , tcpState      = Closed
  , tcpAcceptors  = Seq.empty
  , tcpNotify     = Nothing
  , tcpSndNxt     = 0
  , tcpSndUna     = 0

  , tcpUserClosed = False
  , tcpOut        = emptyRemoteWindow sendWindow
  , tcpOutBuffer  = emptyBuffer 16384
  , tcpOutMSS     = defaultMSS
  , tcpIn         = emptyLocalWindow 0
  , tcpInBuffer   = emptyBuffer 16384
  , tcpInMSS      = defaultMSS

  , tcpTimers     = emptyTcpTimers
  , tcpTimestamp  = Nothing
  }

defaultMSS :: Int64
defaultMSS  = 1460

tcpRcvNxt :: TcpSocket -> TcpSeqNum
tcpRcvNxt = lwRcvNxt . tcpIn

nextSegSize :: TcpSocket -> Int64
nextSegSize tcp =
  min (fromIntegral (rwAvailable (tcpOut tcp))) (tcpOutMSS tcp)

isAccepting :: TcpSocket -> Bool
isAccepting  = not . Seq.null . tcpAcceptors

needsDelayedAck :: TcpSocket -> Bool
needsDelayedAck  = ttDelayedAck . tcpTimers

mkMSS :: TcpSocket -> TcpOption
mkMSS tcp = OptMaxSegmentSize (fromIntegral (tcpInMSS tcp))

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
