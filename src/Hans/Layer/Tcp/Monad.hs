{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Layer.Tcp.Monad where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Types
import Hans.Layer.Timer
import Hans.Message.Tcp

import Control.Monad (MonadPlus(..),guard)
import MonadLib (get,set,StateT,runStateT,inBase)
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sequence as Seq


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

-- | Send out a tcp segment via the IP layer.
sendSegment :: IP4 -> TcpHeader -> L.ByteString -> Tcp ()
sendSegment dst hdr body = do
  ip4 <- ip4Handle
  output $ withIP4Source ip4 dst $ \ src ->
    let pkt = renderWithTcpChecksumIP4 src dst hdr body
     in sendIP4Packet ip4 tcpProtocol dst pkt


-- Socket Monad ----------------------------------------------------------------

newtype Sock a = Sock
  { unSock :: StateT TcpSocket Tcp a
  } deriving (Functor,Monad,MonadPlus)

inTcp :: Tcp a -> Sock a
inTcp  = Sock . inBase

runSock :: SocketId -> TcpSocket -> Sock a -> Tcp a
runSock sid tcp (Sock m) = do
  (a,tcp') <- runStateT tcp m
  addConnection sid tcp'
  return a

newConnection :: SocketId -> TcpSocket -> Sock a -> Tcp a
newConnection  = runSock

listeningConnection :: SocketId -> Sock a -> Tcp a
listeningConnection sid m = do
  tcp      <- getConnection sid
  guard (tcpState tcp == Listen && isAccepting tcp)
  runSock sid tcp m

-- | Run a socket operation in the context of the socket identified by the
-- socket id.
establishedConnection :: SocketId -> Sock a -> Tcp a
establishedConnection sid m = do
  tcp <- getConnection sid
  runSock sid tcp m

-- | Get the parent id of the current socket, and fail if it doesn't exist.
getParent :: Sock SocketId
getParent  = do
  tcp <- getTcpSocket
  case tcpParent tcp of
    Just sid -> return sid
    Nothing  -> mzero

-- | Run an action in the context of the socket's parent.  Fail the whole
-- computation if no parent exists.
inParent :: Sock a -> Sock a
inParent m = do
  pid <- getParent
  inTcp $ do
    p <- getConnection pid
    runSock pid p m

withChild :: TcpSocket -> Sock a -> Sock a
withChild tcp m = inTcp (runSock (tcpSocketId tcp) tcp m)

getTcpSocket :: Sock TcpSocket
getTcpSocket  = Sock get

setTcpSocket :: TcpSocket -> Sock ()
setTcpSocket tcp = Sock (set tcp)

modifyTcpSocket :: (TcpSocket -> (a,TcpSocket)) -> Sock a
modifyTcpSocket k = Sock $ do
  tcp <- get
  let (a,tcp') = k tcp
  set $! tcp'
  return a

modifyTcpSocket_ :: (TcpSocket -> TcpSocket) -> Sock ()
modifyTcpSocket_ k = modifyTcpSocket (\tcp -> ((), k tcp))

-- | Set the state of the current connection.
setState :: ConnState -> Sock ()
setState state = modifyTcpSocket_ (\tcp -> tcp { tcpState = state })

-- | Get the state of the current connection.
getState :: Sock ConnState
getState  = tcpState `fmap` getTcpSocket

pushAcceptor :: Acceptor -> Sock ()
pushAcceptor k = modifyTcpSocket_ $ \ tcp -> tcp
  { tcpAcceptors = tcpAcceptors tcp Seq.|> k
  }

-- | Pop off an acceptor, failing if none exist.
popAcceptor :: Sock Acceptor
popAcceptor  = do
  tcp <- getTcpSocket
  case Seq.viewl (tcpAcceptors tcp) of
    a Seq.:< as -> do
      setTcpSocket $! tcp { tcpAcceptors = as }
      return a
    Seq.EmptyL -> mzero

pushClose :: Close -> Sock ()
pushClose k = modifyTcpSocket_ $ \ tcp -> tcp
  { tcpClose = tcpClose tcp Seq.|> k
  }

-- | Output some IO to the Tcp layer.
outputS :: IO () -> Sock ()
outputS  = inTcp . output

advanceRcvNxt :: TcpSeqNum -> Sock ()
advanceRcvNxt n = modifyTcpSocket_ (\tcp -> tcp { tcpRcvNxt = tcpRcvNxt tcp + n })

advanceSndNxt :: TcpSeqNum -> Sock ()
advanceSndNxt n = modifyTcpSocket_ (\tcp -> tcp { tcpSndNxt = tcpSndNxt tcp + n })

runClosed :: Sock ()
runClosed  = do
  tcp <- getTcpSocket
  modifyTcpSocket_ (\tcp' -> tcp' { tcpClose = Seq.empty })
  outputS (F.sequence_ (tcpClose tcp))
