module Hans.Layer.Tcp.Monad where

import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Types
import Hans.Layer.Timer

import Control.Monad (mzero)
import MonadLib (get,set)
import qualified Data.Map as Map


-- TCP Monad -------------------------------------------------------------------

type TcpHandle = Channel (Tcp ())

type Tcp = Layer TcpState

type Connections = Map.Map SocketId TcpSocket

data TcpState = TcpState
  { tcpSelf   :: TcpHandle
  , tcpIP4    :: IP4Handle
  , tcpTimers :: TimerHandle
  , tcpConns  :: Connections
  }

emptyTcpState :: TcpHandle -> IP4Handle -> TimerHandle -> TcpState
emptyTcpState tcp ip4 timer = TcpState
  { tcpSelf   = tcp
  , tcpIP4    = ip4
  , tcpTimers = timer
  , tcpConns  = Map.empty
  }

-- | The handle to this layer.
self :: Tcp TcpHandle
self  = tcpSelf `fmap` get

-- | Get the handle to the IP4 layer.
ip4Handle :: Tcp IP4Handle
ip4Handle  = tcpIP4 `fmap` get

-- | Get the handle to the Timer layer.
timerHandle :: Tcp TimerHandle
timerHandle  = tcpTimers `fmap` get

getConnections :: Tcp Connections
getConnections  = tcpConns `fmap` get

setConnections :: Connections -> Tcp ()
setConnections cons = do
  rw <- get
  set $! rw { tcpConns = cons }

lookupConnection :: SocketId -> Tcp (Maybe TcpSocket)
lookupConnection sid = do
  cons <- getConnections
  return (Map.lookup sid cons)

getConnection :: SocketId -> Tcp TcpSocket
getConnection sid = do
  cs <- getConnections
  case Map.lookup sid cs of
    Just tcp -> return tcp
    Nothing  -> mzero

setConnection :: SocketId -> TcpSocket -> Tcp ()
setConnection ident con = do
  cons <- getConnections
  setConnections (Map.insert ident con cons)

newConnection :: SocketId -> ConnState -> Tcp ()
newConnection sid state = addConnection sid emptyTcpSocket { tcpState = state }

addConnection :: SocketId -> TcpSocket -> Tcp ()
addConnection sid tcp = do
  cons <- getConnections
  setConnections (Map.insert sid tcp cons)

modifyConnection :: SocketId -> (TcpSocket -> TcpSocket) -> Tcp ()
modifyConnection sid k = do
  cons <- getConnections
  setConnections (Map.adjust k sid cons)

remConnection :: SocketId -> Tcp ()
remConnection sid = do
  cons <- getConnections
  setConnections (Map.delete sid cons)
