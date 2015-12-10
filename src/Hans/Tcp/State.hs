{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Tcp.State where

import           Hans.Addr (Addr,wildcardAddr)
import           Hans.Config (HasConfig(..),Config(..))
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Network.Types (RouteInfo(..))
import           Hans.Tcp.Packet
import           Hans.Tcp.Tcb
import           Hans.Time

import           Control.Concurrent (forkIO,threadDelay)
import qualified Data.Foldable as F
import           Data.Hashable (Hashable)
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Time.Clock (getCurrentTime,UTCTime,diffUTCTime,addUTCTime)
import           GHC.Generics (Generic)


-- General State ---------------------------------------------------------------

data ListenKey = ListenKey !Addr !TcpPort
                 deriving (Show,Eq,Ord,Generic)

data Key = Key !Addr !TcpPort !Addr !TcpPort
           deriving (Show,Eq,Ord,Generic)

instance Hashable ListenKey
instance Hashable Key

data TcpState =
  TcpState { tcpListen   :: {-# UNPACK #-} !(HT.HashTable ListenKey ListenTcb)
           , tcpActive   :: {-# UNPACK #-} !(HT.HashTable Key Tcb)
           , tcpTimeWait :: {-# UNPACK #-} !(IORef (ExpireHeap TimeWaitTcb))
           }


class HasTcpState state where
  tcpState :: Getting r state TcpState

instance HasTcpState TcpState where
  tcpState = id
  {-# INLINE tcpState #-}


newTcpState :: Config -> IO TcpState
newTcpState Config { .. } =
  do tcpListen   <- HT.newHashTable cfgTcpListenTableSize
     tcpActive   <- HT.newHashTable cfgTcpActiveTableSize
     tcpTimeWait <- newIORef emptyHeap
     return TcpState { .. }


-- | Register a new listening socket.
registerListening :: HasTcpState state
                  => state -> Addr -> TcpPort -> ListenTcb -> IO ()
registerListening state addr port val =
  HT.insert (ListenKey addr port) val (tcpListen (view tcpState state))


-- | Register a new active socket.
registerActive :: HasTcpState state
               => state -> Addr -> TcpPort -> Addr -> TcpPort -> Tcb -> IO ()
registerActive state dst dstPort src srcPort val =
  HT.insert (Key dst dstPort src srcPort) val (tcpActive (view tcpState state))


-- | Register a socket in the TimeWait state. If the heap was empty, fork off a
-- thread to reap its contents after the timeWaitTimeout.
--
-- NOTE: this doesn't remove the original socket from the Active set.
registerTimeWait :: (HasConfig state, HasTcpState state)
                 => state -> TimeWaitTcb -> IO ()
registerTimeWait state tcb =
  do let Config { .. } = view config state
     now      <- getCurrentTime
     mbExpire <-
       atomicModifyIORef' (tcpTimeWait (view tcpState state)) $ \ heap ->
           let (heap', next) = expireAt (addUTCTime cfgTcpTimeoutTimeWait now) tcb heap

               shouldReap | nullHeap heap = Just next
                          | otherwise     = Nothing

            in (heap', shouldReap)

     -- mbExpire will only be Just in the case that the heap was previously
     -- empty. Because we're only ever using atomicModifyIORef to mutate the
     -- heap, this will only happen when this context is the only one that
     -- observed the heap being empty. As a result, we can safely spawn a thread
     -- without the danger of spawning on for each time a socket is registered
     -- in TimeWait.
     case mbExpire of
       Just future -> do _ <- forkIO $ do delayDiff now future
                                          reapLoop
                         return ()

       Nothing -> return ()

  where

  -- delay by at least half a second, until some point in the future.
  delayDiff now future =
    threadDelay (max 500000 (toUSeconds (diffUTCTime future now)))

  -- The reap thread will reap TimeWait sockets according to their expiration
  -- time, and then exit.
  reapLoop =
    do now      <- getCurrentTime
       mbExpire <-
         atomicModifyIORef' (tcpTimeWait (view tcpState state)) $ \ heap ->
             let heap' = dropExpired now heap
              in (heap', nextEvent heap')

       case mbExpire of
         Just future -> do delayDiff now future
                           reapLoop

         Nothing     -> return ()


-- | Lookup a socket in the Listen state.
lookupListening :: HasTcpState state
                => state -> Addr -> TcpPort -> IO (Maybe ListenTcb)
lookupListening state src port =
  do mb <- HT.lookup (ListenKey src port) (tcpListen (view tcpState state))
     case mb of
       Just {} -> return mb
       Nothing ->
         HT.lookup (ListenKey (wildcardAddr src) port) (tcpListen (view tcpState state))


-- | Lookup an active socket.
lookupActive :: HasTcpState state
             => state -> Addr -> TcpPort -> Addr -> TcpPort -> IO (Maybe Tcb)
lookupActive state dst dstPort src srcPort =
  HT.lookup (Key dst dstPort src srcPort) (tcpActive (view tcpState state))


-- | Lookup a socket in the TimeWait state.
lookupTimeWait :: HasTcpState state
             => state -> Addr -> TcpPort -> Addr -> TcpPort
             -> IO (Maybe TimeWaitTcb)
lookupTimeWait state dst dstPort src srcPort =
  do heap <- readIORef (tcpTimeWait (view tcpState state))
     return (payload `fmap` F.find isConn heap)
  where
  isConn Entry { payload = TimeWaitTcb { .. } } =
    and [ twDest               == dst
        , twDestPort           == dstPort
        , riSource twRouteInfo == src
        , twSourcePort         == srcPort ]

