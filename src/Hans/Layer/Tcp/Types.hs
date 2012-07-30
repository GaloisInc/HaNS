module Hans.Layer.Tcp.Types where

import Hans.Address.IP4
import Hans.Message.Tcp

import Control.Exception
import Data.Word (Word16)
import qualified Data.Sequence as Seq


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

data TcpSocket = TcpSocket
  { tcpParent      :: Maybe SocketId
  , tcpSocketId    :: !SocketId
  , tcpState       :: !ConnState
  , tcpAcceptors   :: Seq.Seq Acceptor
  , tcpClose       :: Seq.Seq Close
  , tcpSndNxt      :: !TcpSeqNum
  , tcpSndUna      :: !TcpSeqNum
  , tcpRcvNxt      :: !TcpSeqNum
  , tcpSockWin     :: !Word16
  , tcpNeedsDelAck :: Bool
  }

emptyTcpSocket :: TcpSocket
emptyTcpSocket  = TcpSocket
  { tcpParent      = Nothing
  , tcpSocketId    = emptySocketId
  , tcpState       = Closed
  , tcpAcceptors   = Seq.empty
  , tcpClose       = Seq.empty
  , tcpSndNxt      = 0
  , tcpSndUna      = 0
  , tcpRcvNxt      = 0
  , tcpSockWin     = 0
  , tcpNeedsDelAck = False
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
