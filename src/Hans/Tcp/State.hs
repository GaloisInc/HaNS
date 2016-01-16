{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module Hans.Tcp.State (
    -- * Tcp State
    HasTcpState(..), TcpState(),
    newTcpState,

    -- ** Listen Sockets
    incrSynBacklog,
    decrSynBacklog,
    registerListening,
    lookupListening,
    deleteListening,

    -- ** Active Sockets
    Key(), tcbKey,
    tcpActive,
    lookupActive,
    registerActive,
    closeActive,
    deleteActive,

    -- ** TimeWait Sockets
    registerTimeWait,
    lookupTimeWait,
    resetTimeWait,
    deleteTimeWait,

    -- ** Port Management
    nextTcpPort,

    -- ** Sequence Numbers
    nextIss,
  ) where

import           Hans.Addr (Addr,wildcardAddr,putAddr)
import           Hans.Config (HasConfig(..),Config(..))
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Network.Types (RouteInfo(..))
import           Hans.Tcp.Packet
import           Hans.Tcp.Tcb
import           Hans.Threads (forkNamed)
import           Hans.Time

import           Control.Concurrent (threadDelay,MVar,newMVar,modifyMVar)
import           Control.Monad (guard)
import           Crypto.Hash (hash,Digest,MD5)
import           Data.ByteArray (withByteArray)
import qualified Data.ByteString as S
import qualified Data.Foldable as F
import           Data.Hashable (Hashable)
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Serialize (runPut,putByteString)
import           Data.Time.Clock (UTCTime,getCurrentTime,addUTCTime,diffUTCTime)
import           Data.Word (Word32)
import           Foreign.Storable (peek)
import           GHC.Generics (Generic)
import           System.Random (newStdGen,random,randoms)


-- General State ---------------------------------------------------------------

data ListenKey = ListenKey !Addr !TcpPort
                 deriving (Show,Eq,Ord,Generic)

listenKey :: Getting r ListenTcb ListenKey
listenKey  = to (\ ListenTcb { .. } -> ListenKey lSrc lPort)
{-# INLINE listenKey #-}


data Key = Key !Addr    -- ^ Remote address
               !TcpPort -- ^ Remote port
               !Addr    -- ^ Local address
               !TcpPort -- ^ Local port
           deriving (Show,Eq,Ord,Generic)

tcbKey :: Getting r Tcb Key
tcbKey  = to $ \Tcb { tcbRouteInfo = RouteInfo { .. }, .. } ->
                Key tcbRemote tcbRemotePort riSource tcbLocalPort
{-# INLINE tcbKey #-}



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

           , tcpPorts       :: {-# UNPACK #-} !(MVar TcpPort)
           , tcpISSTimer    :: {-# UNPACK #-} !(IORef Tcp4USTimer)
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



data Tcp4USTimer = Tcp4USTimer { tcpTimer      :: {-# UNPACK #-} !Word32
                               , tcpSecret     :: {-# UNPACK #-} !S.ByteString
                               , tcpLastUpdate :: !UTCTime
                               }

newTcp4USTimer :: IO Tcp4USTimer
newTcp4USTimer  =
  do tcpLastUpdate <- getCurrentTime
     gen           <- newStdGen
     let (tcpTimer,gen') = random gen
         tcpSecret       = S.pack (take 256 (randoms gen'))
     return Tcp4USTimer { .. }



newTcpState :: Config -> IO TcpState
newTcpState Config { .. } =
  do tcpListen_     <- HT.newHashTable cfgTcpListenTableSize
     tcpActive_     <- HT.newHashTable cfgTcpActiveTableSize
     tcpTimeWait_   <- newIORef emptyHeap
     tcpSynBacklog_ <- newIORef cfgTcpMaxSynBacklog
     tcpPorts       <- newMVar 32767
     tcpISSTimer    <- newIORef =<< newTcp4USTimer
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


-- Listening Sockets -----------------------------------------------------------

-- | Register a new listening socket.
registerListening :: HasTcpState state
                  => state -> ListenTcb -> IO Bool
registerListening state tcb =
  HT.alter update (view listenKey tcb) (view tcpListen state)
  where
  update Nothing   = (Just tcb, True)
  update mb@Just{} = (mb, False)
{-# INLINE registerListening #-}


-- | Remove a listening socket.
deleteListening :: HasTcpState state
                => state -> ListenTcb -> IO ()
deleteListening state tcb =
  HT.delete (view listenKey tcb) (view tcpListen state)
{-# INLINE deleteListening #-}


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


-- TimeWait Sockets ------------------------------------------------------------

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
       Just reaper -> do _ <- forkNamed "TimeWait Reaper" reaper
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


-- | Lookup a socket in the TimeWait state.
lookupTimeWait :: HasTcpState state
             => state -> Addr -> TcpPort -> Addr -> TcpPort
             -> IO (Maybe TimeWaitTcb)
lookupTimeWait state dst dstPort src srcPort =
  do heap <- readIORef (view tcpTimeWait state)
     return (payload `fmap` F.find isConn heap)
  where
  isConn Entry { payload = TimeWaitTcb { .. } } =
    and [ twRemote             == dst
        , twRemotePort         == dstPort
        , riSource twRouteInfo == src
        , twLocalPort          == srcPort ]
{-# INLINE lookupTimeWait #-}


-- | Delete an entry from the TimeWait heap.
deleteTimeWait :: HasTcpState state => state -> TimeWaitTcb -> IO ()
deleteTimeWait state tw =
  atomicModifyIORef' (view tcpTimeWait state) $ \ heap ->
      (filterHeap (/= tw) heap, ())
{-# INLINE deleteTimeWait #-}


-- Active Sockets --------------------------------------------------------------

-- | Register a new active socket.
registerActive :: HasTcpState state => state -> Tcb -> IO Bool
registerActive state tcb =
  HT.alter update (view tcbKey tcb) (view tcpActive state)
  where
  update Nothing = (Just tcb, True)
  update mb      = (mb, False)
{-# INLINE registerActive #-}


-- | Lookup an active socket.
lookupActive :: HasTcpState state
             => state -> Addr -> TcpPort -> Addr -> TcpPort -> IO (Maybe Tcb)
lookupActive state dst dstPort src srcPort =
  HT.lookup (Key dst dstPort src srcPort) (view tcpActive state)
{-# INLINE lookupActive #-}


-- | Delete the 'Tcb', and notify any waiting processes.
closeActive :: HasTcpState state => state -> Tcb -> IO ()
closeActive state tcb =
  do finalizeTcb tcb
     deleteActive state tcb
{-# INLINE closeActive #-}


-- | Delete an active connection from the tcp state.
deleteActive :: HasTcpState state => state -> Tcb -> IO ()
deleteActive state tcb =
  HT.delete (view tcbKey tcb) (view tcpActive state)
{-# INLINE deleteActive #-}


-- Port Management -------------------------------------------------------------

-- | Pick a fresh port for a connection.
nextTcpPort :: HasTcpState state
            => state -> Addr -> Addr -> TcpPort -> IO (Maybe TcpPort)
nextTcpPort state src dst dstPort =
  modifyMVar tcpPorts (pickFreshPort tcpActive_ (Key dst dstPort src))
  where
  TcpState { .. } = view tcpState state

pickFreshPort :: HT.HashTable Key Tcb -> (TcpPort -> Key) -> TcpPort
              -> IO (TcpPort, Maybe TcpPort)
pickFreshPort ht mkKey p0 = go 0 p0
  where

  go :: Int -> TcpPort -> IO (TcpPort,Maybe TcpPort)
  go i _ | i > 65535 = return (p0, Nothing)
  go i 0             = go (i+1) 1025
  go i port          =
    do used <- HT.hasKey (mkKey port) ht
       if not used
          then return (port, Just port)
          else go (i + 1) (port + 1)


-- Sequence Numbers ------------------------------------------------------------

nextIss :: HasTcpState state
        => state -> Addr -> TcpPort -> Addr -> TcpPort -> IO TcpSeqNum
nextIss state src srcPort dst dstPort =
  do let TcpState { .. } = view tcpState state
     now <- getCurrentTime
     (m,f_digest) <- atomicModifyIORef' tcpISSTimer $ \ Tcp4USTimer { .. } ->
       let diff    = diffUTCTime now tcpLastUpdate
           ticks   = tcpTimer + truncate (diff * 250000) -- 4us chunks
           timers' = Tcp4USTimer { tcpTimer      = ticks
                                 , tcpLastUpdate = now
                                 , .. }

           digest :: Digest MD5
           digest  = hash $ runPut $
             do putAddr src
                putTcpPort srcPort
                putAddr dst
                putTcpPort dstPort
                putByteString tcpSecret

        in (timers', (ticks, digest))

     -- NOTE: MD5 digests are always 128 bytes, so peeking the first 4 bytes off
     -- of the front should never fail.
     withByteArray f_digest $ \ ptr ->
       do w32 <- peek ptr
          return (fromIntegral (m + w32))
