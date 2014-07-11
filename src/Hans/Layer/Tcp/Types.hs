{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Layer.Tcp.Types where

import Hans.Address.IP4
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp
import Hans.Ports

import Control.Exception (Exception,SomeException,toException)
import Control.Monad ( guard )
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word16,Word32)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq


-- Hosts Information -----------------------------------------------------------

data Host = Host
  { hostConnections   :: Connections
  , hostTimeWaits     :: TimeWaitConnections
  , hostInitialSeqNum :: !TcpSeqNum
  , hostPorts         :: !(PortManager TcpPort)
  , hostLastUpdate    :: POSIXTime
  }

emptyHost :: POSIXTime -> Host
emptyHost start = Host
  { hostConnections   = Map.empty
  , hostTimeWaits     = Map.empty
    -- XXX what should we seed this with?
  , hostInitialSeqNum = 0
  , hostPorts         = emptyPortManager [32768 .. 61000]
  , hostLastUpdate    = start
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


-- Connections in TimeWait -----------------------------------------------------

-- | The socket that's in TimeWait, plus its current 2MSL value.
type TimeWaitConnections = Map.Map SocketId TimeWaitSock

data TimeWaitSock = TimeWaitSock { tw2MSL     :: !SlowTicks
                                   -- ^ The current 2MSL value
                                 , twInit2MSL :: !SlowTicks
                                   -- ^ The initial 2MSL value
                                 , twSeqNum   :: !TcpSeqNum
                                   -- ^ The sequence number to use when
                                   -- responding to messages
                                 , twRcvNxt   :: !TcpSeqNum
                                   -- ^ The next expected sequence number from
                                   -- the remote host
                                 } deriving (Show)

twReset2MSL :: TimeWaitSock -> TimeWaitSock
twReset2MSL tw = tw { tw2MSL = twInit2MSL tw }

mkTimeWait :: TcpSocket -> TimeWaitSock
mkTimeWait TcpSocket { .. } =
  TimeWaitSock { tw2MSL     = timeout
               , twInit2MSL = timeout
               , twSeqNum   = tcpSndNxt
               , twRcvNxt   = lwRcvNxt tcpIn
               }
  where
  timeout | tt2MSL tcpTimers <= 0 = 2 * mslTimeout
          | otherwise             = tt2MSL tcpTimers

-- | Add a socket to the TimeWait map.
addTimeWait :: TcpSocket -> TimeWaitConnections -> TimeWaitConnections
addTimeWait tcp socks = Map.insert (tcpSocketId tcp) (mkTimeWait tcp) socks

-- | Take one step, collecting any connections whose 2MSL timer goes to 0.
stepTimeWaitConnections :: TimeWaitConnections -> TimeWaitConnections
stepTimeWaitConnections  = Map.mapMaybe $ \ tw ->
  do guard (tw2MSL tw > 0)
     return tw { tw2MSL = tw2MSL tw - 1 }


-- Timers ----------------------------------------------------------------------

type SlowTicks = Int

-- | MSL is 60 seconds, which is slightly more aggressive than the 2 minutes
-- from the original RFC.
mslTimeout :: SlowTicks
mslTimeout = 2 * 60

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
  , tsGranularity   = 200
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
  { tcpParent      :: Maybe SocketId
  , tcpSocketId    :: !SocketId
  , tcpState       :: !ConnState
  , tcpAcceptors   :: Seq.Seq Acceptor
  , tcpNotify      :: Maybe Notify
  , tcpIss         :: !TcpSeqNum
  , tcpSndNxt      :: !TcpSeqNum
  , tcpSndUna      :: !TcpSeqNum

  , tcpUserClosed  :: Bool
  , tcpOut         :: RemoteWindow
  , tcpOutBuffer   :: Buffer Outgoing
  , tcpOutMSS      :: !Int64
  , tcpIn          :: LocalWindow
  , tcpInBuffer    :: Buffer Incoming
  , tcpInMSS       :: !Int64

  , tcpTimers      :: !TcpTimers
  , tcpTimestamp   :: Maybe Timestamp

  , tcpSack        :: Bool
  , tcpWindowScale :: Bool
  }

emptyTcpSocket :: Word16 -> Int -> TcpSocket
emptyTcpSocket sendWindow sendScale = TcpSocket
  { tcpParent      = Nothing
  , tcpSocketId    = emptySocketId
  , tcpState       = Closed
  , tcpAcceptors   = Seq.empty
  , tcpNotify      = Nothing
  , tcpIss         = 0
  , tcpSndNxt      = 0
  , tcpSndUna      = 0

  , tcpUserClosed  = False
  , tcpOut         = emptyRemoteWindow sendWindow sendScale
  , tcpOutBuffer   = emptyBuffer 16384
  , tcpOutMSS      = defaultMSS
  , tcpIn          = emptyLocalWindow 0 14600 0
  , tcpInBuffer    = emptyBuffer 16384
  , tcpInMSS       = defaultMSS

  , tcpTimers      = emptyTcpTimers
  , tcpTimestamp   = Nothing

  , tcpSack        = True
  , tcpWindowScale = True
  }

defaultMSS :: Int64
defaultMSS  = 1460

nothingOutstanding :: TcpSocket -> Bool
nothingOutstanding TcpSocket { .. } = tcpSndUna == tcpSndNxt

tcpRcvNxt :: TcpSocket -> TcpSeqNum
tcpRcvNxt = lwRcvNxt . tcpIn

inRcvWnd :: TcpSeqNum -> TcpSocket -> Bool
inRcvWnd (TcpSeqNum sn) TcpSocket { .. } =
  rcvNxt <= sn && sn < rcvNxt + fromIntegral (lwRcvWind tcpIn)
  where
  TcpSeqNum rcvNxt = lwRcvNxt tcpIn

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
