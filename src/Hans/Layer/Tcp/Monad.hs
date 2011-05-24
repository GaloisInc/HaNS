module Hans.Layer.Tcp.Monad where

import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Timer

import MonadLib
import Network.TCP.Type.Datagram (IPMessage)
import Network.TCP.Type.Socket (Host(..),empty_host,TCPSocket)
import Network.TCP.Type.Syscall (SocketID)
import qualified Data.Map as Map


-- TCP Monad -------------------------------------------------------------------

type TcpHandle = Channel (Tcp ())

type Tcp = Layer (TcpState (IO ()))

data TcpState t = TcpState
  { tcpSelf   :: TcpHandle
  , tcpIP4    :: IP4Handle
  , tcpTimers :: TimerHandle
  , tcpHost   :: Host t
  }

emptyTcpState :: TcpHandle -> IP4Handle -> TimerHandle -> TcpState t
emptyTcpState tcp ip4 timer = TcpState
  { tcpSelf   = tcp
  , tcpIP4    = ip4
  , tcpTimers = timer
  , tcpHost   = empty_host
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


-- Compatibility Layer ---------------------------------------------------------

type HMonad t = Layer (TcpState t)

get_host :: HMonad t (Host t)
get_host  = tcpHost `fmap` get

put_host :: Host t -> HMonad t ()
put_host h = do
  s <- get
  set $! s { tcpHost = h }

modify_host :: (Host t -> Host t) -> HMonad t ()
modify_host f = do
  h <- get_host
  put_host $! f h

emit_segs :: [IPMessage] -> HMonad t ()
emit_segs segs = modify_host (\h -> h { output_queue = output_queue h ++ segs })

emit_ready :: [t] -> HMonad t ()
emit_ready ts = modify_host (\h -> h { ready_list = ready_list h ++ ts })

has_sock :: SocketID -> HMonad t Bool
has_sock sid = (Map.member sid . sock_map) `fmap` get_host

lookup_sock :: SocketID -> HMonad t (TCPSocket t)
lookup_sock sid = do
  h <- get_host
  case Map.lookup sid (sock_map h) of
    Nothing  -> fail "lookup_sock: sid not found"
    Just res -> return res

delete_sock :: SocketID -> HMonad t ()
delete_sock sid =
  modify_host (\h -> h { sock_map = Map.delete sid (sock_map h) } )

update_sock :: SocketID -> (TCPSocket t -> TCPSocket t) -> HMonad t ()
update_sock sid f =
  modify_host (\h -> h { sock_map = Map.adjust f sid (sock_map h) })

insert_sock :: SocketID -> TCPSocket t -> HMonad t ()
insert_sock sid sock = do
  modify_host (\h -> h { sock_map = Map.insert sid sock (sock_map h) })
