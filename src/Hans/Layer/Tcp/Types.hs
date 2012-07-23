module Hans.Layer.Tcp.Types where

import Hans.Address.IP4
import Hans.Message.Tcp

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

data SocketRequest
  = SockListen
    deriving (Show)

data SocketResult a
  = SocketResult a
  | SocketError
    deriving (Show)

type Acceptor = SocketId -> IO ()

data TcpSocket = TcpSocket
  { tcpSocketId  :: !SocketId
  , tcpState     :: !ConnState
  , tcpAcceptors :: Seq.Seq Acceptor
  }

emptyTcpSocket :: TcpSocket
emptyTcpSocket  = TcpSocket
  { tcpSocketId  = emptySocketId
  , tcpState     = Closed
  , tcpAcceptors = Seq.empty
  }

isAccepting :: TcpSocket -> Bool
isAccepting  = not . Seq.null . tcpAcceptors

pushAcceptor :: Acceptor -> TcpSocket -> TcpSocket
pushAcceptor k tcp = tcp { tcpAcceptors = tcpAcceptors tcp Seq.|> k }

popAcceptor :: TcpSocket -> Maybe (Acceptor,TcpSocket)
popAcceptor tcp = case Seq.viewl (tcpAcceptors tcp) of
  k Seq.:< ks -> Just (k,tcp { tcpAcceptors = ks })
  Seq.EmptyL  -> Nothing

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
