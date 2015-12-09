{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Tcp.State where

import           Hans.Addr (Addr,wildcardAddr)
import           Hans.Config (Config(..))
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Network.Types (RouteInfo)
import           Hans.Tcp.Packet

import           Data.Hashable (Hashable)
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import           Data.Time.Clock (getCurrentTime,UTCTime,diffUTCTime)
import           GHC.Generics (Generic)


-- General State ---------------------------------------------------------------

data Key = Listening !Addr !TcpPort
           -- ^ Listening connections
           --
           -- XXX: allow for the remote side to be specified

         | Conn !Addr !TcpPort !Addr !TcpPort
           -- ^ Open connections

           deriving (Show,Eq,Ord,Generic)

instance Hashable Key


data TcpState =
  TcpState { tcpSockets :: {-# UNPACK #-} !(HT.HashTable Key TcbState)
           }


class HasTcpState state where
  tcpState :: Getting r state TcpState

instance HasTcpState TcpState where
  tcpState = id
  {-# INLINE tcpState #-}


newTcpState :: Config -> IO TcpState
newTcpState Config { .. } =
  do tcpSockets <- HT.newHashTable cfgTcpSocketTableSize
     return TcpState { .. }


registerSocket :: HasTcpState state => state -> Key -> TcbState -> IO ()
registerSocket state key val =
  HT.insert key val (tcpSockets (view tcpState state))


-- Tcb -------------------------------------------------------------------------

data TcbState = Listen !ListenTcb
              | SynReceived !Tcb
              | SynSent !Tcb
              | Established !Tcb
              | Closed


data IssGen = IssGen { issLastSeqNum :: !TcpSeqNum
                     , issLastUpdate :: !UTCTime
                     }

-- | Update the ISS, based on a 128khz incrementing counter.
genIss :: UTCTime -> IssGen -> (IssGen,TcpSeqNum)
genIss now IssGen { .. } = (IssGen iss' now, iss')
  where
  increment = round (diffUTCTime now issLastUpdate * 128000)
  iss'      = issLastSeqNum + increment


data ListenTcb = ListenTcb { ltcbIss  :: !(IORef IssGen)
                           , ltcbSrc  :: !Addr
                           , ltcbPort :: !TcpPort
                           }

nextIss :: ListenTcb -> IO TcpSeqNum
nextIss ListenTcb { .. } =
  do now <- getCurrentTime
     atomicModifyIORef' ltcbIss (genIss now)


data Tcb = Tcb { tcbSndUna
               , tcbSndNxt
               , tcbSndWnd
               , tcbSndUp
               , tcbSndWl1
               , tcbSndWl2
               , tcbIss
               , tcbRcvNxt
               , tcbRcvWnd
               , tcbRcvUp
               , tcbIrs :: !(IORef TcpSeqNum)

                 -- Port information
               , tcbSourcePort
               , tcbDestPort :: !TcpPort

                 -- routing information
               , tcbRouteInfo :: !(RouteInfo Addr)
               , tcbDest      :: !Addr
               }


newTcb :: RouteInfo Addr -> TcpPort -> Addr -> TcpPort -> IO Tcb
newTcb tcbRouteInfo tcbSourcePort tcbDest tcbDestPort =
  do tcbSndUna <- newIORef 0
     tcbSndNxt <- newIORef 0
     tcbSndWnd <- newIORef 0
     tcbSndUp  <- newIORef 0
     tcbSndWl1 <- newIORef 0
     tcbSndWl2 <- newIORef 0
     tcbIss    <- newIORef 0
     tcbRcvNxt <- newIORef 0
     tcbRcvWnd <- newIORef 0
     tcbRcvUp  <- newIORef 0
     tcbIrs    <- newIORef 0
     return Tcb { .. }


incVar :: IORef TcpSeqNum -> TcpSeqNum -> IO ()
incVar ref n = atomicModifyIORef' ref (\ c -> (c + n, ()))


-- | Lookup the Tcb for an established connection.
findEstablished :: HasTcpState state
                => state -> Addr -> TcpPort -> Addr -> TcpPort -> IO (Maybe TcbState)
findEstablished state src srcPort dst dstPort =
  HT.lookup (Conn src srcPort dst dstPort) (tcpSockets (view tcpState state))


-- | Find the Tcb for a listening connection.
findListening :: HasTcpState state => state -> Addr -> TcpPort -> IO (Maybe TcbState)
findListening state src srcPort =
  do mb <- HT.lookup (Listening src srcPort) tcpSockets
     case mb of
       Just{}  -> return mb
       Nothing -> HT.lookup (Listening (wildcardAddr src) srcPort) tcpSockets

  where
  TcpState { .. } = view tcpState state


-- | Find a Tcb.
findTcb :: HasTcpState state
        => state -> Addr -> TcpPort -> Addr -> TcpPort -> IO TcbState
findTcb state src srcPort dst dstPort =
  do mb <- findEstablished state src srcPort dst dstPort
     case mb of
       Just tcb -> return tcb
       Nothing  -> do mb' <- findListening state dst dstPort
                      case mb' of
                        Just tcb -> return tcb
                        Nothing  -> return Closed
