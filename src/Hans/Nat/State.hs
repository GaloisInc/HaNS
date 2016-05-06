{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Hans.Nat.State (
    NatState(), HasNatState(..),
    newNatState,
    Flow(..),
    TcpSession(..),
    UdpSession(..),

    HasSession(..), otherSide,

    PortForward(..),

    -- ** Rules
    addUdpPortForward, removeUdpPortForward,
    addTcpPortForward, removeTcpPortForward,

    -- ** Queries
    udpForwardingActive, addUdpSession, shouldForwardUdp,
    tcpForwardingActive, addTcpSession, shouldForwardTcp,
  ) where

import           Hans.Addr (Addr,isWildcardAddr)
import qualified Hans.HashTable as HT
import           Hans.Lens (Getting,view)
import           Hans.Network.Types (RouteInfo(..))
import           Hans.Tcp.Packet (TcpPort)
import           Hans.Threads (forkNamed)
import           Hans.Udp.Packet (UdpPort)

import           Control.Concurrent
                     (ThreadId,threadDelay,MVar,newMVar,modifyMVar_)
import           Control.Monad (forever,when)
import           Data.Hashable (Hashable)
import           Data.IORef
                     (IORef,newIORef,atomicWriteIORef,readIORef
                     ,atomicModifyIORef')
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
                       , flowRemote     :: !Addr
                       , flowRemotePort :: !Word16
                       } deriving (Functor,Eq,Generic)

instance Hashable remote => Hashable (Flow remote)

data TcpSession = TcpSession { tsFinAt       :: !(IORef (Maybe UTCTime))
                             , tsLastMessage :: !(IORef UTCTime)
                             , tsLeft        :: !(Flow (RouteInfo Addr))
                             , tsRight       :: !(Flow (RouteInfo Addr))
                             }

data UdpSession = UdpSession { usLastMessage :: !(IORef UTCTime)
                             , usLeft        :: !(Flow (RouteInfo Addr))
                             , usRight       :: !(Flow (RouteInfo Addr))
                             }

class HasSession sess where
  sessionFlows :: sess -> (Flow (RouteInfo Addr), Flow (RouteInfo Addr))
  touchSession :: sess -> IO ()
  sessionAge   :: sess -> IO UTCTime

instance HasSession TcpSession where
  sessionFlows TcpSession { .. } = (tsLeft, tsRight)

  touchSession TcpSession { .. } =
    do now <- getCurrentTime
       atomicWriteIORef tsLastMessage now

  sessionAge TcpSession { .. } = 
    do mbFin <- readIORef tsFinAt
       case mbFin of
         Just t  -> return t
         Nothing -> readIORef tsLastMessage 


instance HasSession UdpSession where
  sessionFlows UdpSession { .. } = (usLeft, usRight)

  touchSession UdpSession { .. } =
    do now <- getCurrentTime
       atomicWriteIORef usLastMessage now

  sessionAge UdpSession { .. } =
    readIORef usLastMessage

-- | Gives back the other end of the session.
otherSide :: HasSession sess => Flow Addr -> sess -> Flow (RouteInfo Addr)
otherSide flow sess =
  let (a,b) = sessionFlows sess
   in if flowRemote flow == flowRemote a then b else a

data NatState =
  NatState { natTcpTable_ :: !(NatTable TcpSession)
             -- ^ Active TCP flows

           , natTcpRules_ :: !(IORef [PortForward])
             -- ^ Ports that have been forwarded in the TCP layer

           , natUdpTable_ :: !(NatTable UdpSession)
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

data PortForward = PortForward { pfSourceAddr :: !Addr
                                 -- ^ Local address to listen on

                               , pfSourcePort :: !Word16
                                 -- ^ The port on this network stack to
                                 -- forward

                               , pfDestAddr :: !Addr
                                 -- ^ Destination machine to forward to

                               , pfDestPort :: !Word16
                                 -- ^ Destination port to forward to
                               }


newNatState :: IO NatState
newNatState  =
  do natTcpTable_ <- newNatTable
     natTcpRules_ <- newIORef []
     natUdpTable_ <- newNatTable
     natUdpRules_ <- newIORef []
     natReaper_   <- forkNamed "Nat.reaper" (reaper natTcpTable_ natUdpTable_)
     return NatState { .. }


-- Nat Tables ------------------------------------------------------------------

data NatTable a = NatTable { natTable :: !(HT.HashTable (Flow Addr) a)
                           , natSize  :: !(MVar Int)
                           }

newNatTable :: HasSession a => IO (NatTable a)
newNatTable  =
  do natTable <- HT.newHashTable 11
     natSize  <- newMVar 0
     return NatTable { .. }

-- | Insert an entry into the NAT table. Increments the size by one.
insertNatTable :: HasSession a => a -> NatTable a -> IO ()
insertNatTable sess NatTable { .. } = modifyMVar_ natSize $ \ size ->
  do let (a,b) = sessionFlows sess
     let add _ = (Just sess, ())

     HT.alter add (fmap riSource a) natTable
     HT.alter add (fmap riSource b) natTable

     return $! size + 1

-- | Remove entries from the NAT table, decrementing the size by the number of
-- entries that were removed.
expireEntries :: HasSession a => UTCTime -> NatTable a -> IO ()
expireEntries now NatTable { .. } =
  do keys <- newIORef []
     let push k = atomicModifyIORef' keys (\ks -> (k:ks,()))

     let shouldExpire k a =
           do lastUpdate <- sessionAge a
              when (addUTCTime fourMinutes lastUpdate <= now) (push k)

     modifyMVar_ natSize $ \ size ->
       do HT.mapHashTableM_ shouldExpire natTable

          expired <- readIORef keys
          HT.deletes expired natTable

          return $! size - length expired

-- | Lookup and touch an entry in the NAT table.
lookupNatTable :: HasSession a => Flow Addr -> NatTable a -> IO (Maybe a)
lookupNatTable key NatTable { .. } =
  do mb <- HT.lookup key natTable
     case mb of
       Just entry -> do touchSession entry
                        return mb
       Nothing    -> return Nothing


-- Table Reaping ---------------------------------------------------------------

-- | Every two minutes, reap old entries from the TCP and UDP NAT tables.
reaper :: NatTable TcpSession -> NatTable UdpSession -> IO ()
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
removeTcpPortForward :: HasNatState state => state -> Addr -> TcpPort -> IO ()
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
removeUdpPortForward :: HasNatState state => state -> Addr -> UdpPort -> IO ()
removeUdpPortForward state addr port =
  do let NatState { .. } = view natState state
     atomicModifyIORef' natUdpRules_ (\rs -> (filter keepRule rs, ()))
  where
  keepRule PortForward { .. } = pfSourceAddr /= addr || pfSourcePort /= port


-- Queries ---------------------------------------------------------------------

-- | Lookup information about an active forwarding session.
tcpForwardingActive :: HasNatState state
                    => state -> Flow Addr -> IO (Maybe TcpSession)
tcpForwardingActive state key =
  do let NatState { .. } = view natState state
     lookupNatTable key natTcpTable_


-- | Lookup information about an active forwarding session.
udpForwardingActive :: HasNatState state
                    => state -> Flow Addr -> IO (Maybe UdpSession)
udpForwardingActive state key =
  do let NatState { .. } = view natState state
     lookupNatTable key natUdpTable_


-- | Insert a TCP forwarding entry into the NAT state.
addTcpSession :: HasNatState state => state -> TcpSession -> IO ()
addTcpSession state sess =
  do let NatState { .. } = view natState state
     insertNatTable sess natTcpTable_


-- | Insert a UDP forwarding entry into the NAT state.
addUdpSession :: HasNatState state => state -> UdpSession -> IO ()
addUdpSession state sess =
  do let NatState { .. } = view natState state
     insertNatTable sess natUdpTable_

ruleApplies :: Flow Addr -> PortForward -> Bool
ruleApplies Flow { .. } = \ PortForward { .. } ->
  flowLocalPort == pfSourcePort &&
  (flowLocal == pfSourceAddr || isWildcardAddr pfSourceAddr)


-- | Returns the forwarding rule to use, if this connection should be forwarded.
shouldForwardTcp :: HasNatState state
                 => state -> Flow Addr -> IO (Maybe PortForward)
shouldForwardTcp state flow =
  do let NatState { .. } = view natState state
     rules <- readIORef natTcpRules_
     return $! find (ruleApplies flow) rules


-- | Returns the forwarding rule to use, if this session should be forwarded
shouldForwardUdp :: HasNatState state
                 => state -> Flow Addr -> IO (Maybe PortForward)
shouldForwardUdp state flow =
  do let NatState { .. } = view natState state
     rules <- readIORef natUdpRules_
     return $! find (ruleApplies flow) rules
