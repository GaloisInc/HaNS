module Hans.Layer.Tcp.Monad where

import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Connection
import Hans.Layer.Timer

import MonadLib (get,set)


-- TCP Monad -------------------------------------------------------------------

type TcpHandle = Channel (Tcp ())

type Tcp = Layer TcpState

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
  , tcpConns  = emptyConnections
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
setConnections conns = do
  rw <- get
  set $! rw { tcpConns = conns }
