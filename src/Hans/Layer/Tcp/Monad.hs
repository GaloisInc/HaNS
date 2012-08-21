{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Layer.Tcp.Monad where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Layer.Timer
import Hans.Message.Tcp

import Control.Monad (MonadPlus(..),guard)
import MonadLib (get,set,StateT,runStateT,inBase)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T


-- TCP Monad -------------------------------------------------------------------

type TcpHandle = Channel (Tcp ())

type Tcp = Layer TcpState

data TcpState = TcpState
  { tcpSelf      :: TcpHandle
  , tcpIP4       :: IP4Handle
  , tcpTimers    :: TimerHandle
  , tcpHost      :: Host
  }

emptyTcpState :: TcpHandle -> IP4Handle -> TimerHandle -> TcpState
emptyTcpState tcp ip4 timer = TcpState
  { tcpSelf   = tcp
  , tcpIP4    = ip4
  , tcpTimers = timer
  , tcpHost   = emptyHost
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


-- Host Operations -------------------------------------------------------------

getHost :: Tcp Host
getHost  = tcpHost `fmap` get

setHost :: Host -> Tcp ()
setHost host = do
  rw <- get
  set $! rw { tcpHost = host }

modifyHost :: (Host -> Host) -> Tcp ()
modifyHost f = do
  host <- getHost
  setHost $! f host

getConnections :: Tcp Connections
getConnections  = hostConnections `fmap` getHost

setConnections :: Connections -> Tcp ()
setConnections cons = modifyHost (\host -> host { hostConnections = cons })

-- | Lookup a connection, returning @Nothing@ if the connection doesn't exist.
lookupConnection :: SocketId -> Tcp (Maybe TcpSocket)
lookupConnection sid = do
  cons <- getConnections
  return (Map.lookup sid cons)

-- | Retrieve a connection from the host.  The computation fails if the
-- connection doesn't exist.
getConnection :: SocketId -> Tcp TcpSocket
getConnection sid = do
  cs <- getConnections
  case Map.lookup sid cs of
    Just tcp -> return tcp
    Nothing  -> mzero

-- | Assign a connection to a socket id.
setConnection :: SocketId -> TcpSocket -> Tcp ()
setConnection ident con = do
  cons <- getConnections
  setConnections (Map.insert ident con cons)

-- | Add a new connection to the host.
addConnection :: SocketId -> TcpSocket -> Tcp ()
addConnection  = setConnection

-- | Modify an existing connection in the host.
modifyConnection :: SocketId -> (TcpSocket -> TcpSocket) -> Tcp ()
modifyConnection sid k = do
  cons <- getConnections
  setConnections (Map.adjust k sid cons)

-- | Remove a connection from the host.
remConnection :: SocketId -> Tcp ()
remConnection sid = do
  cons <- getConnections
  setConnections (Map.delete sid cons)

-- | Send out a tcp segment via the IP layer.
sendSegment :: IP4 -> TcpHeader -> L.ByteString -> Tcp ()
sendSegment dst hdr body = do
  ip4 <- ip4Handle
  output $ withIP4Source ip4 dst $ \ src -> do
    let pkt = renderWithTcpChecksumIP4 src dst hdr body
    sendIP4Packet ip4 tcpProtocol dst pkt

-- | Get the initial sequence number.
initialSeqNum :: Tcp TcpSeqNum
initialSeqNum  = hostInitialSeqNum `fmap` getHost

-- | Increment the initial sequence number by a value.
addInitialSeqNum :: TcpSeqNum -> Tcp ()
addInitialSeqNum sn =
  modifyHost (\host -> host { hostInitialSeqNum = hostInitialSeqNum host + sn })

-- | Allocate a new port for use.
allocatePort :: Tcp TcpPort
allocatePort  = do
  host <- getHost
  case takePort host of
    Just (p,host') -> do
      setHost host'
      return p
    Nothing -> mzero

-- | Release a used port.
closePort :: TcpPort -> Tcp ()
closePort port = modifyHost (releasePort port)


-- Socket Monad ----------------------------------------------------------------

newtype Sock a = Sock
  { unSock :: StateT TcpSocket Tcp a
  } deriving (Functor,Monad,MonadPlus)

inTcp :: Tcp a -> Sock a
inTcp  = Sock . inBase

runSock :: TcpSocket -> Sock a -> Tcp a
runSock tcp (Sock m) = do
  (a,tcp') <- runStateT tcp m
  addConnection (tcpSocketId tcp') tcp'
  return a

-- | Iterate for each connection, rolling back to its previous state if the
-- computation fails.
eachConnection :: Sock () -> Tcp ()
eachConnection (Sock body) =
  setConnections . removeClosed =<< T.mapM sandbox =<< getConnections
  where
  sandbox tcp = (snd `fmap` runStateT tcp body) `mplus` return tcp

listeningConnection :: SocketId -> Sock a -> Tcp a
listeningConnection sid m = do
  tcp <- getConnection sid
  guard (tcpState tcp == Listen && isAccepting tcp)
  runSock tcp m

-- | Run a socket operation in the context of the socket identified by the
-- socket id.
--
-- XXX this should really be renamed, as it's not guarding on the state of the
-- socket
establishedConnection :: SocketId -> Sock a -> Tcp a
establishedConnection sid m = do
  tcp <- getConnection sid
  runSock tcp m

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
    runSock p m

withChild :: TcpSocket -> Sock a -> Sock a
withChild tcp m = inTcp (runSock tcp m)

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

-- | Send a notification back to a waiting process that the socket has been
-- established, or that it has failed.  It's assumed that this will only be
-- called from the context of a user socket, so when the parameter is @False@,
-- the user close field will be set to true.
notify :: Bool -> Sock ()
notify success = do
  mbNotify <- modifyTcpSocket $ \ tcp ->
    let tcp' = tcp { tcpNotify = Nothing }
     in if success
           then (tcpNotify tcp, tcp')
           else (tcpNotify tcp, tcp' { tcpUserClosed = True })

  case mbNotify of
    Just f  -> outputS (f success)
    Nothing -> mzero

-- | Output some IO to the Tcp layer.
outputS :: IO () -> Sock ()
outputS  = inTcp . output

advanceRcvNxt :: TcpSeqNum -> Sock ()
advanceRcvNxt n = modifyTcpSocket_ (\tcp -> tcp { tcpRcvNxt = tcpRcvNxt tcp + n })

advanceSndNxt :: TcpSeqNum -> Sock ()
advanceSndNxt n = modifyTcpSocket_ (\tcp -> tcp { tcpSndNxt = tcpSndNxt tcp + n })

remoteHost :: Sock IP4
remoteHost  = (sidRemoteHost . tcpSocketId) `fmap` getTcpSocket

-- | Send a TCP segment in the context of a socket.
tcpOutput :: TcpHeader -> L.ByteString -> Sock ()
tcpOutput hdr body = do
  dst <- remoteHost
  inTcp (sendSegment dst hdr body)

-- | Set the socket state to closed, and unblock any waiting processes.
closeSocket :: Sock ()
closeSocket  = do
  fin <- modifyTcpSocket $ \ tcp -> 
      let (wOut,bufOut) = shutdownWaiting (tcpOutBuffer tcp)
          (wIn,bufIn)   = shutdownWaiting (tcpInBuffer tcp)
       in (wOut >> wIn,tcp { tcpOutBuffer = bufOut, tcpInBuffer = bufIn })
  outputS fin
  setState Closed
