{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Hans.Layer.Tcp.Monad where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Ip4
import Hans.Message.Tcp

import Control.Applicative(Applicative(..))
import Control.Monad (MonadPlus(..),guard,when)
import Data.Time.Clock.POSIX (POSIXTime)
import MonadLib (get,set)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq


-- TCP Monad -------------------------------------------------------------------

type TcpHandle = Channel (Tcp ())

type Tcp = Layer TcpState

data TcpState = TcpState
  { tcpSelf  :: {-# UNPACK #-} !TcpHandle
  , tcpIP4   :: {-# UNPACK #-} !IP4Handle
  , tcpHost  :: !Host
  }

emptyTcpState :: TcpHandle -> IP4Handle -> POSIXTime -> TcpState
emptyTcpState tcp ip4 start = TcpState
  { tcpSelf  = tcp
  , tcpIP4   = ip4
  , tcpHost  = emptyHost start
  }

-- | The handle to this layer.
self :: Tcp TcpHandle
self  = tcpSelf `fmap` get

-- | Get the handle to the IP4 layer.
ip4Handle :: Tcp IP4Handle
ip4Handle  = tcpIP4 `fmap` get


-- Host Operations -------------------------------------------------------------

getHost :: Tcp Host
getHost  = tcpHost `fmap` get

getLastUpdate :: Tcp POSIXTime
getLastUpdate  = hostLastUpdate `fmap` getHost

setHost :: Host -> Tcp ()
setHost host = do
  rw <- get
  host `seq` set rw { tcpHost = host }

modifyHost_ :: (Host -> Host) -> Tcp ()
modifyHost_ f = do
  host <- getHost
  setHost (f host)

modifyHost :: (Host -> (a,Host)) -> Tcp a
modifyHost f = do
  host <- getHost
  let (a,host') = f host
  setHost host'
  return a

-- | Reset the 2MSL timer on the socket in TimeWait.
resetTimeWait2MSL :: SocketId -> Tcp ()
resetTimeWait2MSL sid = modifyHost_ $ \ host ->
  host { hostTimeWaits = Map.adjust twReset2MSL sid (hostTimeWaits host) }

getTimeWait :: IP4 -> TcpHeader -> Tcp (Maybe (SocketId,TimeWaitSock))
getTimeWait remote hdr =
  do host <- getHost
     let sid = incomingSocketId remote hdr
     return $ do tw <- Map.lookup sid (hostTimeWaits host)
                 return (sid,tw)

removeTimeWait :: SocketId -> Tcp ()
removeTimeWait sid =
  modifyHost_ $ \ host ->
    host { hostTimeWaits = Map.delete sid (hostTimeWaits host) }

getConnections :: Tcp Connections
getConnections  = hostConnections `fmap` getHost

takeConnections :: Tcp Connections
takeConnections  =
  modifyHost $ \ Host { .. } ->
    (hostConnections, Host { hostConnections = Map.empty, .. })

setConnections :: Connections -> Tcp ()
setConnections cons = modifyHost_ (\host -> host { hostConnections = cons })

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

-- | Assign a connection to a socket id.  If the TcpSocket is in TimeWait, this
-- will do two things:
--
--  1. Remove the corresponding key from the connections map
--  2. Add the socket to the TimeWait map, using the current value of its 2MSL
--     timer (which should be set when the TimeWait state is entered)
--
-- The purpose of this is to clean up the memory associated with the connection
-- as soon as possible, and once it's in TimeWait, no data will flow on the
-- socket.
setConnection :: SocketId -> TcpSocket -> Tcp ()
setConnection ident con
  | tcpState con == TimeWait =
    modifyHost_ $ \ host ->
      host { hostTimeWaits   = addTimeWait con (hostTimeWaits host)
           , hostConnections = Map.delete ident (hostConnections host)
           }

  | otherwise =
    do cons <- getConnections
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
    let ip4Hdr = emptyIP4Header
          { ip4DestAddr     = dst
          , ip4Protocol     = tcpProtocol
          , ip4DontFragment = False
          }
        pkt    = renderWithTcpChecksumIP4 src dst hdr body
    sendIP4Packet ip4 ip4Hdr pkt

-- | Get the initial sequence number.
initialSeqNum :: Tcp TcpSeqNum
initialSeqNum  = hostInitialSeqNum `fmap` getHost

-- | Increment the initial sequence number by a value.
addInitialSeqNum :: TcpSeqNum -> Tcp ()
addInitialSeqNum sn =
  modifyHost_ (\host -> host { hostInitialSeqNum = hostInitialSeqNum host + sn })

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
closePort port = modifyHost_ (releasePort port)


-- Socket Monad ----------------------------------------------------------------

-- | Tcp operations in the context of a socket.
--
-- This implementation is a bit ridiculous, and when the eventual rewrite comes
-- this should be one of the first things to be reconsidered.  The basic problem
-- is that if you rely on the `finished` implementation for the Layer monad, you
-- exit from the socket context as well, losing any changes that have been made
-- locally.  This gives the ability to simulate `finished`, with the benefit of
-- only yielding from the Sock context, not the whole Tcp context.
newtype Sock a = Sock
  { unSock :: forall r. TcpSocket -> Escape r -> Next a r -> Tcp r
  }

type Escape r = TcpSocket      -> Tcp r
type Next a r = TcpSocket -> a -> Tcp r

instance Functor Sock where
  fmap f m = Sock $ \s  x k -> unSock m s x
                  $ \s' a   -> k s' (f a)
  {-# INLINE fmap #-}

instance Applicative Sock where
  {-# INLINE pure #-}
  pure x = Sock $ \ s _ k -> k s x

  {-# INLINE (<*>) #-}
  f <*> a = Sock $ \ s   x k -> unSock f s  x
                 $ \ s'  g   -> unSock a s' x
                 $ \ s'' b   -> k s'' (g b)

instance Monad Sock where
  {-# INLINE return #-}
  return = pure

  m >>= f = Sock $ \ s  x k -> unSock m s x
                 $ \ s' a   -> unSock (f a) s' x k
  {-# INLINE (>>=) #-}

  m >> n = Sock $ \ s  x k -> unSock m s  x
                $ \ s' _   -> unSock n s' x k
  {-# INLINE (>>) #-}

inTcp :: Tcp a -> Sock a
inTcp m = Sock $ \ s _ k -> do a <- m
                               k s a
{-# INLINE inTcp #-}


-- | Finish early, with no result.
escape :: Sock a
escape  = Sock $ \ s x _ -> x s

runSock_ :: TcpSocket -> Sock a -> Tcp ()
runSock_ tcp sm =
  do _ <- runSock tcp sm
     return ()

runSock :: TcpSocket -> Sock a -> Tcp (Maybe a)
runSock tcp sm =
  do (tcp',mb) <- runSock' tcp sm
     setConnection (tcpSocketId tcp') $! tcp'
     return mb

seqMaybe :: Maybe a -> ()
seqMaybe (Just a) = a `seq` ()
seqMaybe Nothing  = ()

-- | Run the socket action, and increment its internal timestamp value.  Do not
-- add it back to the connections map.
runSock' :: TcpSocket -> Sock a -> Tcp (TcpSocket,Maybe a)
runSock' tcp sm = do
  now      <- time
  let steppedTcp = tcp { tcpTimestamp =
                           let ts' = stepTimestamp now `fmap` tcpTimestamp tcp
                            in seqMaybe ts' `seq` ts'
                       }
  res@(tcp',_) <- (unSock sm $! steppedTcp) escapeK nextK
  tcp' `seq` return res
  where
  escapeK s = return (s,Nothing)
  nextK s a = return (s,Just a)

-- | Iterate for each connection, rolling back to its previous state if the
-- computation fails.
eachConnection :: Sock () -> Tcp ()
eachConnection m =
  do cs       <- takeConnections
     (cs',ws) <- sandbox [] [] (Map.elems cs)

     modifyHost_ $ \ Host { .. } ->
       Host { hostConnections = cs'
            , hostTimeWaits   = Map.union ws hostTimeWaits
            , ..
            }

  where

  -- Prevent failure in the socket action from leaking out of this scope.  When
  -- failure is detected, just return the old TCB
  sandbox active timeWait (tcp:rest) =
    do tcp' <- fmap fst (runSock' tcp m) `mplus` return tcp
       if tcpState tcp' == TimeWait
          then sandbox       active (tcp':timeWait) rest
          else sandbox (tcp':active)      timeWait  rest

  sandbox active timeWait [] =
    return ( Map.fromList [ (tcpSocketId tcp, tcp)            | tcp <- active ]
           , Map.fromList [ (tcpSocketId tcp, mkTimeWait tcp) | tcp <- timeWait ])

withConnection :: IP4 -> TcpHeader -> Sock a -> Tcp ()
withConnection remote hdr m = withConnection' remote hdr m mzero

withConnection' :: IP4 -> TcpHeader -> Sock a -> Tcp () -> Tcp ()
withConnection' remote hdr m noConn = do
  cs <- getConnections
  case Map.lookup estId cs `mplus` Map.lookup listenId cs of
    Just con -> runSock_ con m
    Nothing  -> noConn
  where
  estId    = incomingSocketId remote hdr
  listenId = listenSocketId (tcpDestPort hdr)

listeningConnection :: SocketId -> Sock a -> Tcp (Maybe a)
listeningConnection sid m = do
  tcp <- getConnection sid
  guard (tcpState tcp == Listen && isAccepting tcp)
  mb <- runSock tcp m
  return mb

-- | Run a socket operation in the context of the socket identified by the
-- socket id.
--
-- XXX this should really be renamed, as it's not guarding on the state of the
-- socket
establishedConnection :: SocketId -> Sock a -> Tcp ()
establishedConnection sid m = do
  tcp <- getConnection sid
  runSock_ tcp m

-- | Get the parent id of the current socket, and fail if it doesn't exist.
getParent :: Sock (Maybe SocketId)
getParent  = tcpParent `fmap` getTcpSocket

-- | Run an action in the context of the socket's parent.  Returns `Nothing` if
-- the connection has no parent.
inParent :: Sock a -> Sock (Maybe a)
inParent m = do
  mbPid <- getParent
  case mbPid of
    Just pid -> inTcp $ do p  <- getConnection pid
                           mb <- runSock p m
                           return mb
    Nothing  -> return Nothing

withChild :: TcpSocket -> Sock a -> Sock (Maybe a)
withChild tcp m = inTcp $ do mb <- runSock tcp m
                             return mb

getTcpSocket :: Sock TcpSocket
getTcpSocket  = Sock (\s _ k -> k s $! s)
{-# INLINE getTcpSocket #-}

setTcpSocket :: TcpSocket -> Sock ()
setTcpSocket tcp = Sock (\ _ _ k -> (k $! tcp) ())
{-# INLINE setTcpSocket #-}

getTcpTimers :: Sock TcpTimers
getTcpTimers  = tcpTimers `fmap` getTcpSocket

modifyTcpSocket :: (TcpSocket -> (a,TcpSocket)) -> Sock a
modifyTcpSocket f = Sock $ \ s _ k -> let (a,s') = f s
                                       in (k $! s') a

modifyTcpSocket_ :: (TcpSocket -> TcpSocket) -> Sock ()
modifyTcpSocket_ k = modifyTcpSocket (\tcp -> ((), k tcp))

modifyTcpTimers :: (TcpTimers -> (a,TcpTimers)) -> Sock a
modifyTcpTimers k = modifyTcpSocket $ \ tcp ->
  let (a,t') = k (tcpTimers tcp)
   in (a,tcp { tcpTimers = t' })

modifyTcpTimers_ :: (TcpTimers -> TcpTimers) -> Sock ()
modifyTcpTimers_ k = modifyTcpTimers (\t -> ((), k t))

-- | Set the state of the current connection.
setState :: ConnState -> Sock ()
setState state = modifyTcpSocket_ (\tcp -> tcp { tcpState = state })

-- | Get the state of the current connection.
getState :: Sock ConnState
getState  = tcpState `fmap` getTcpSocket

whenState :: ConnState -> Sock () -> Sock ()
whenState state body = do
  curState <- getState
  when (state == curState) body

whenStates :: [ConnState] -> Sock () -> Sock ()
whenStates states body = do
  curState <- getState
  when (curState `elem` states) body

pushAcceptor :: Acceptor -> Sock ()
pushAcceptor k = modifyTcpSocket_ $ \ tcp -> tcp
  { tcpAcceptors = tcpAcceptors tcp Seq.|> k
  }

-- | Pop off an acceptor.
popAcceptor :: Sock (Maybe Acceptor)
popAcceptor  = do
  tcp <- getTcpSocket
  case Seq.viewl (tcpAcceptors tcp) of
    a Seq.:< as -> do setTcpSocket $! tcp { tcpAcceptors = as }
                      return (Just a)
    Seq.EmptyL  -> return Nothing

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
    Nothing -> return ()

-- | Output some IO to the Tcp layer.
outputS :: IO () -> Sock ()
outputS  = inTcp . output

advanceRcvNxt :: TcpSeqNum -> Sock ()
advanceRcvNxt n =
  modifyTcpSocket_ (\tcp -> tcp { tcpIn = addRcvNxt n (tcpIn tcp) })

advanceSndNxt :: TcpSeqNum -> Sock ()
advanceSndNxt n =
  modifyTcpSocket_ (\tcp -> tcp { tcpSndNxt = tcpSndNxt tcp + n })

remoteHost :: Sock IP4
remoteHost  = (sidRemoteHost . tcpSocketId) `fmap` getTcpSocket

-- | Send a TCP segment in the context of a socket.
tcpOutput :: TcpHeader -> L.ByteString -> Sock ()
tcpOutput hdr body = do
  dst <- remoteHost
  inTcp (sendSegment dst hdr body)


-- | Unblock any waiting processes, in preparation to close.
shutdown :: Sock ()
shutdown  = do
  finalize <- modifyTcpSocket $ \ tcp -> 
      let (wOut,bufOut) = flushWaiting (tcpOutBuffer tcp)
          (wIn,bufIn)   = flushWaiting (tcpInBuffer tcp)
       in (wOut >> wIn,tcp { tcpOut       = clearRetransmit (tcpOut tcp)
                           , tcpOutBuffer = bufOut
                           , tcpInBuffer  = bufIn
                           })
  outputS finalize

-- | Set the socket state to closed, and unblock any waiting processes.
closeSocket :: Sock ()
closeSocket  = do
  shutdown
  setState Closed
