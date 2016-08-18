{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Hans.Nat.State (
    NatState(), HasNatState(..),
    newNatState,
    Flow(..),
    Session(..),

    otherSide,

    PortForward(..),

    -- ** Rules
    addUdpPortForward, removeUdpPortForward,
    addTcpPortForward, removeTcpPortForward,

    -- ** Queries
    udpForwardingActive, addUdpSession, shouldForwardUdp,
    tcpForwardingActive, addTcpSession, shouldForwardTcp,
  ) where

import           Hans.Addr (IP4,isWildcard)
import           Hans.Config (Config(..))
import           Hans.Lens (Getting,view)
import           Hans.Network.Types (RouteInfo(..))
import           Hans.Tcp.Packet (TcpPort)
import           Hans.Threads (forkNamed)
import           Hans.Udp.Packet (UdpPort)

import           Control.Concurrent (ThreadId,threadDelay)
import           Control.Monad (forever)
import           Data.HashPSQ as Q
import           Data.Hashable (Hashable)
import           Data.IORef (IORef,newIORef,readIORef,atomicModifyIORef')
import           Data.List (find)
import           Data.Time.Clock
                     (UTCTime,getCurrentTime,NominalDiffTime,addUTCTime)
import           Data.Word (Word16)
import           GHC.Generics (Generic)


-- State -----------------------------------------------------------------------

-- | NOTE: as TcpPort and UdpPort are both type aliases to Word16, Flow isn't
-- parameterized on the port type.
data Flow local = Flow { flowLocal      :: !local
                       , flowLocalPort  :: !Word16
                       , flowRemote     :: !IP4
                       , flowRemotePort :: !Word16
                       } deriving (Functor,Eq,Ord,Generic,Show)

instance Hashable remote => Hashable (Flow remote)


data NatState =
  NatState { natTcpTable_ :: !NatTable
             -- ^ Active TCP flows

           , natTcpRules_ :: !(IORef [PortForward])
             -- ^ Ports that have been forwarded in the TCP layer

           , natUdpTable_ :: !NatTable
             -- ^ Active UDP flows

           , natUdpRules_ :: !(IORef [PortForward])
             -- ^ Ports that have been forwarded in the UDP layer

           , natReaper_   :: !ThreadId
             -- ^ When flows are active, this is the id of the reaping thread
           }

class HasNatState state where
  natState :: Getting r state NatState

instance HasNatState NatState where
  natState = id

data PortForward = PortForward { pfSourceAddr :: !IP4
                                 -- ^ Local address to listen on

                               , pfSourcePort :: !Word16
                                 -- ^ The port on this network stack to
                                 -- forward

                               , pfDestAddr :: !IP4
                                 -- ^ Destination machine to forward to

                               , pfDestPort :: !Word16
                                 -- ^ Destination port to forward to
                               }


newNatState :: Config -> IO NatState
newNatState cfg =
  do natTcpTable_ <- newNatTable cfg
     natTcpRules_ <- newIORef []
     natUdpTable_ <- newNatTable cfg
     natUdpRules_ <- newIORef []
     natReaper_   <- forkNamed "Nat.reaper" (reaper natTcpTable_ natUdpTable_)
     return NatState { .. }


-- Nat Tables ------------------------------------------------------------------

data Session = Session { sessLeft, sessRight :: !(Flow (RouteInfo IP4)) }

-- | Gives back the other end of the session.
otherSide :: Flow IP4 -> Session -> Flow (RouteInfo IP4)
otherSide flow Session { .. } =
  if flowRemote flow == flowRemote sessLeft
     && flowRemotePort flow == flowRemotePort sessLeft
     then sessRight else sessLeft

sessionFlows :: Session -> (Flow IP4, Flow IP4)
sessionFlows Session { .. } = (fmap riSource sessLeft, fmap riSource sessRight)


type Sessions = Q.HashPSQ (Flow IP4) UTCTime Session

addSession :: UTCTime -> Session -> Sessions -> Sessions
addSession age a q =
  let (l,r) = sessionFlows a
   in Q.insert l age a (Q.insert r age a q)

removeOldest :: Sessions -> Sessions
removeOldest q =
  case Q.minView q of
    Just (k,_,a,q') -> Q.delete (fmap riSource (otherSide k a)) q'
    Nothing         -> q

removeSession :: Flow IP4 -> Sessions -> Maybe (Session,Sessions)
removeSession flow q =
  case Q.deleteView flow q of
    Just (_,a,q') -> Just (a,Q.delete (fmap riSource (otherSide flow a)) q')
    Nothing       -> Nothing


data NatTable = NatTable { natConfig :: Config
                         , natTable  :: !(IORef Sessions)
                         }

newNatTable :: Config -> IO NatTable
newNatTable natConfig =
  do natTable <- newIORef Q.empty
     return NatTable { .. }

-- | Insert an entry into the NAT table.
insertNatTable :: Session -> NatTable -> IO ()
insertNatTable sess NatTable { .. } =
  do now <- getCurrentTime
     atomicModifyIORef' natTable $ \ q ->
       let q' = addSession now sess q
        in if Q.size q' > cfgNatMaxEntries natConfig
              then (removeOldest q', ())
              else (q', ())

-- | Remove entries from the NAT table, decrementing the size by the number of
-- entries that were removed.
expireEntries :: UTCTime -> NatTable -> IO ()
expireEntries now NatTable { .. } =
  atomicModifyIORef' natTable go
  where
  now' = addUTCTime (negate fourMinutes) now

  -- remove entries that are older than four minutes
  go q =
    case Q.minView q of
      Just (k,p,a,q')
        | p < now'  -> go (Q.delete (fmap riSource (otherSide k a)) q')
        | otherwise -> (q, ())

      Nothing -> (Q.empty, ())

-- | Lookup and touch an entry in the NAT table.
lookupNatTable :: Flow IP4 -> NatTable -> IO (Maybe Session)
lookupNatTable key NatTable { .. } =
  do now <- getCurrentTime
     atomicModifyIORef' natTable $ \ q ->
       case removeSession key q of
         Just (a,q') -> (addSession now a q', Just a)
         Nothing     -> (q, Nothing)


-- Table Reaping ---------------------------------------------------------------

-- | Every two minutes, reap old entries from the TCP and UDP NAT tables.
reaper :: NatTable -> NatTable -> IO ()
reaper tcp udp = forever $
  do threadDelay (2 * 60 * 1000000) -- delay for two minutes

     now <- getCurrentTime
     expireEntries now tcp
     expireEntries now udp

fourMinutes :: NominalDiffTime
fourMinutes  = 4 * 60.0


-- Rules -----------------------------------------------------------------------

addTcpPortForward :: HasNatState state => state -> PortForward -> IO ()
addTcpPortForward state rule =
  do let NatState { .. } = view natState state
     atomicModifyIORef' natTcpRules_ (\rs -> (rule : rs, ()))

-- | Remove port forwarding for UDP based on source address and port number.
removeTcpPortForward :: HasNatState state => state -> IP4 -> TcpPort -> IO ()
removeTcpPortForward state addr port =
  do let NatState { .. } = view natState state
     atomicModifyIORef' natTcpRules_ (\rs -> (filter keepRule rs, ()))
  where
  keepRule PortForward { .. } = pfSourceAddr /= addr || pfSourcePort /= port

addUdpPortForward :: HasNatState state => state -> PortForward -> IO ()
addUdpPortForward state rule =
  do let NatState { .. } = view natState state
     atomicModifyIORef' natUdpRules_ (\rs -> (rule : rs, ()))

-- | Remove port forwarding for UDP based on source address and port number.
removeUdpPortForward :: HasNatState state => state -> IP4 -> UdpPort -> IO ()
removeUdpPortForward state addr port =
  do let NatState { .. } = view natState state
     atomicModifyIORef' natUdpRules_ (\rs -> (filter keepRule rs, ()))
  where
  keepRule PortForward { .. } = pfSourceAddr /= addr || pfSourcePort /= port


-- Queries ---------------------------------------------------------------------

-- | Lookup information about an active forwarding session.
tcpForwardingActive :: HasNatState state
                    => state -> Flow IP4 -> IO (Maybe Session)
tcpForwardingActive state key =
  do let NatState { .. } = view natState state
     lookupNatTable key natTcpTable_


-- | Lookup information about an active forwarding session.
udpForwardingActive :: HasNatState state
                    => state -> Flow IP4 -> IO (Maybe Session)
udpForwardingActive state key =
  do let NatState { .. } = view natState state
     lookupNatTable key natUdpTable_


-- | Insert a TCP forwarding entry into the NAT state.
addTcpSession :: HasNatState state => state -> Session -> IO ()
addTcpSession state sess =
  do let NatState { .. } = view natState state
     insertNatTable sess natTcpTable_


-- | Insert a UDP forwarding entry into the NAT state.
addUdpSession :: HasNatState state => state -> Session -> IO ()
addUdpSession state sess =
  do let NatState { .. } = view natState state
     insertNatTable sess natUdpTable_

ruleApplies :: Flow IP4 -> PortForward -> Bool
ruleApplies Flow { .. } = \ PortForward { .. } ->
  flowLocalPort == pfSourcePort &&
  (flowLocal == pfSourceAddr || isWildcard pfSourceAddr)


-- | Returns the forwarding rule to use, if this connection should be forwarded.
shouldForwardTcp :: HasNatState state
                 => state -> Flow IP4 -> IO (Maybe PortForward)
shouldForwardTcp state flow =
  do let NatState { .. } = view natState state
     rules <- readIORef natTcpRules_
     return $! find (ruleApplies flow) rules


-- | Returns the forwarding rule to use, if this session should be forwarded
shouldForwardUdp :: HasNatState state
                 => state -> Flow IP4 -> IO (Maybe PortForward)
shouldForwardUdp state flow =
  do let NatState { .. } = view natState state
     rules <- readIORef natUdpRules_
     return $! find (ruleApplies flow) rules
