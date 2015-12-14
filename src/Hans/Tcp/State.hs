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
import           Control.Monad (guard)
import qualified Data.Foldable as F
import           Data.Hashable (Hashable)
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Time.Clock (UTCTime,getCurrentTime,addUTCTime,diffUTCTime)
import           GHC.Generics (Generic)


-- General State ---------------------------------------------------------------

data ListenKey = ListenKey !Addr !TcpPort
                 deriving (Show,Eq,Ord,Generic)

data Key = Key !Addr    -- ^ Remote address
               !TcpPort -- ^ Remote port
               !Addr    -- ^ Local address
               !TcpPort -- ^ Local port
           deriving (Show,Eq,Ord,Generic)

instance Hashable ListenKey
instance Hashable Key

type TimeWaitHeap = ExpireHeap TimeWaitTcb

data TcpState =
  TcpState { tcpListen_     :: {-# UNPACK #-} !(HT.HashTable ListenKey ListenTcb)
           , tcpActive_     :: {-# UNPACK #-} !(HT.HashTable Key Tcb)
           , tcpTimeWait_   :: {-# UNPACK #-} !(IORef TimeWaitHeap)
           , tcpSynBacklog_ :: {-# UNPACK #-} !(IORef Int)
             -- ^ Decrements when a connection enters SynReceived or SynSent,
             -- and increments back up once it's closed, or enters Established.
           }

tcpListen :: HasTcpState state => Getting r state (HT.HashTable ListenKey ListenTcb)
tcpListen  = tcpState . to tcpListen_
{-# INLINE tcpListen #-}

tcpActive :: HasTcpState state => Getting r state (HT.HashTable Key Tcb)
tcpActive  = tcpState . to tcpActive_
{-# INLINE tcpActive #-}

tcpTimeWait :: HasTcpState state => Getting r state (IORef TimeWaitHeap)
tcpTimeWait  = tcpState . to tcpTimeWait_
{-# INLINE tcpTimeWait #-}

tcpSynBacklog :: HasTcpState state => Getting r state (IORef Int)
tcpSynBacklog  = tcpState . to tcpSynBacklog_
{-# INLINE tcpSynBacklog #-}


class HasTcpState state where
  tcpState :: Getting r state TcpState

instance HasTcpState TcpState where
  tcpState = id
  {-# INLINE tcpState #-}


newTcpState :: Config -> IO TcpState
newTcpState Config { .. } =
  do tcpListen_     <- HT.newHashTable cfgTcpListenTableSize
     tcpActive_     <- HT.newHashTable cfgTcpActiveTableSize
     tcpTimeWait_   <- newIORef emptyHeap
     tcpSynBacklog_ <- newIORef cfgTcpMaxSynBacklog
     return TcpState { .. }


-- | Returns 'True' when there is space in the Syn backlog, and False if the
-- connection should be rejected.
decrSynBacklog :: HasTcpState state => state -> IO Bool
decrSynBacklog state =
  atomicModifyIORef' (view tcpSynBacklog state) $ \ backlog ->
    if backlog > 0
       then (backlog - 1, True)
       else (backlog, False)

-- | Yield back an entry in the Syn backlog.
incrSynBacklog :: HasTcpState state => state -> IO ()
incrSynBacklog state =
  atomicModifyIORef' (view tcpSynBacklog state)
                     (\ backlog -> (backlog + 1, ()))


-- | Register a new listening socket.
registerListening :: HasTcpState state
                  => state -> Addr -> TcpPort -> ListenTcb -> IO ()
registerListening state addr port val =
  HT.insert (ListenKey addr port) val (view tcpListen state)


-- | Register a new active socket.
registerActive :: HasTcpState state
               => state -> Addr -> TcpPort -> Addr -> TcpPort -> Tcb -> IO ()
registerActive state remote remotePort local localPort val =
  HT.insert (Key remote remotePort local localPort) val (view tcpActive state)


-- | Register a socket in the TimeWait state. If the heap was empty, fork off a
-- thread to reap its contents after the timeWaitTimeout.
--
-- NOTE: this doesn't remove the original socket from the Active set.
registerTimeWait :: (HasConfig state, HasTcpState state)
                 => state -> TimeWaitTcb -> IO ()
registerTimeWait state tcb =
  let Config { .. } = view config state
   in updateTimeWait state $ \ now heap ->
          fst (expireAt (addUTCTime cfgTcpTimeoutTimeWait now) tcb heap)

-- | Reset the timer associated with a TimeWaitTcb.
resetTimeWait :: (HasConfig state, HasTcpState state)
              => state -> TimeWaitTcb -> IO ()
resetTimeWait state tcb =
  let Config { .. } = view config state
   in updateTimeWait state $ \ now heap ->
          fst $ expireAt (addUTCTime cfgTcpTimeoutTimeWait now) tcb
              $ filterHeap (/= tcb) heap

-- | Modify the TimeWait heap, and spawn a reaping thread when necessary.
updateTimeWait :: (HasConfig state, HasTcpState state)
               => state -> (UTCTime -> TimeWaitHeap -> TimeWaitHeap) -> IO ()
updateTimeWait state update =
  do now    <- getCurrentTime
     mbReap <-
       atomicModifyIORef' (view tcpTimeWait state) $ \ heap ->
           let heap'  = update now heap

               -- Return a reaping action if:
               --
               -- 1. The original heap was empty, signifying that there was no
               --    existing reaper running
               --
               -- 2. The user action added something to the heap
               reaper = do guard (nullHeap heap)
                           future <- nextEvent heap'
                           return $ do delayDiff now future
                                       reapLoop

            in (heap', reaper)

     case mbReap of
       Just reaper -> do _ <- forkIO reaper
                         return ()

       Nothing     -> return ()

  where

  -- delay by at least half a second, until some point in the future.
  delayDiff now future =
    threadDelay (max 500000 (toUSeconds (diffUTCTime future now)))

  -- The reap thread will reap TimeWait sockets according to their expiration
  -- time, and then exit.
  reapLoop =
    do now      <- getCurrentTime
       mbExpire <-
         atomicModifyIORef' (view tcpTimeWait state) $ \ heap ->
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
  do mb <- HT.lookup (ListenKey src port) (view tcpListen state)
     case mb of
       Just {} -> return mb
       Nothing ->
         HT.lookup (ListenKey (wildcardAddr src) port) (view tcpListen state)
{-# INLINE lookupListening #-}


-- | Lookup an active socket.
lookupActive :: HasTcpState state
             => state -> Addr -> TcpPort -> Addr -> TcpPort -> IO (Maybe Tcb)
lookupActive state dst dstPort src srcPort =
  HT.lookup (Key dst dstPort src srcPort) (view tcpActive state)
{-# INLINE lookupActive #-}


-- | Lookup a socket in the TimeWait state.
lookupTimeWait :: HasTcpState state
             => state -> Addr -> TcpPort -> Addr -> TcpPort
             -> IO (Maybe TimeWaitTcb)
lookupTimeWait state dst dstPort src srcPort =
  do heap <- readIORef (view tcpTimeWait state)
     return (payload `fmap` F.find isConn heap)
  where
  isConn Entry { payload = TimeWaitTcb { .. } } =
    and [ twDest               == dst
        , twDestPort           == dstPort
        , riSource twRouteInfo == src
        , twSourcePort         == srcPort ]
{-# INLINE lookupTimeWait #-}


tcbKey :: Tcb -> Key
tcbKey Tcb { .. } =
  let RouteInfo { .. } = tcbRouteInfo
   in Key tcbRemote tcbRemotePort riSource tcbLocalPort
{-# INLINE tcbKey #-}


-- | Delete an active connection from the tcp state.
deleteActive :: HasTcpState state => state -> Tcb -> IO ()
deleteActive state tcb =
  HT.delete (tcbKey tcb) (view tcpActive state)
{-# INLINE deleteActive #-}


-- | Delete an entry from the TimeWait heap.
deleteTimeWait :: HasTcpState state => state -> TimeWaitTcb -> IO ()
deleteTimeWait state tw =
  atomicModifyIORef' (view tcpTimeWait state) $ \ heap ->
      (filterHeap (/= tw) heap, ())
