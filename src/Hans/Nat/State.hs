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
import           Hans.Udp.Packet (UdpPort)

import           Data.Hashable (Hashable)
import           Data.IORef
                     (IORef,newIORef,atomicWriteIORef,readIORef
                     ,atomicModifyIORef')
import           Data.List (find)
import           Data.Time.Clock (UTCTime,getCurrentTime)
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

instance HasSession TcpSession where
  sessionFlows TcpSession { .. } = (tsLeft, tsRight)
  touchSession TcpSession { .. } =
    do now <- getCurrentTime
       atomicWriteIORef tsLastMessage now

instance HasSession UdpSession where
  sessionFlows UdpSession { .. } = (usLeft, usRight)
  touchSession UdpSession { .. } =
    do now <- getCurrentTime
       atomicWriteIORef usLastMessage now

-- | Gives back the other end of the session.
otherSide :: HasSession sess => Flow Addr -> sess -> Flow (RouteInfo Addr)
otherSide flow sess =
  let (a,b) = sessionFlows sess
   in if flowRemote flow == flowRemote a then b else a


data NatState =
  NatState { natTcpTable_ :: HT.HashTable (Flow Addr) TcpSession
             -- ^ Active TCP flows

           , natTcpRules_ :: !(IORef [PortForward])
             -- ^ Ports that have been forwarded in the TCP layer

           , natUdpTable_ :: HT.HashTable (Flow Addr) UdpSession
             -- ^ Active UDP flows

           , natUdpRules_ :: !(IORef [PortForward])
             -- ^ Ports that have been forwarded in the UDP layer
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
  do natTcpTable_ <- HT.newHashTable 11
     natTcpRules_ <- newIORef []
     natUdpTable_ <- HT.newHashTable 11
     natUdpRules_ <- newIORef []
     return NatState { .. }


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
     mb <- HT.lookup key natTcpTable_
     case mb of
       Just entry -> do touchSession entry
                        return (Just entry)
       Nothing    -> return Nothing


-- | Lookup information about an active forwarding session.
udpForwardingActive :: HasNatState state
                    => state -> Flow Addr -> IO (Maybe UdpSession)
udpForwardingActive state key =
  do let NatState { .. } = view natState state
     mb <- HT.lookup key natUdpTable_
     case mb of
       Just entry -> do touchSession entry
                        return (Just entry)
       Nothing    -> return Nothing


-- | Insert a TCP forwarding entry into the NAT state.
addTcpSession :: HasNatState state => state -> TcpSession -> IO ()
addTcpSession state sess =
  do let NatState { .. } = view natState state
     let (a,b)           = sessionFlows sess

     let add _           = (Just sess, ())
     HT.alter add (fmap riSource a) natTcpTable_
     HT.alter add (fmap riSource b) natTcpTable_

     -- XXX fork the reaping thread, if it's not already started


-- | Insert a UDP forwarding entry into the NAT state.
addUdpSession :: HasNatState state => state -> UdpSession -> IO ()
addUdpSession state sess =
  do let NatState { .. } = view natState state

     let (a,b)           = sessionFlows sess
         add _           = (Just sess, ())

     HT.alter add (fmap riSource a) natUdpTable_
     HT.alter add (fmap riSource b) natUdpTable_

     -- XXX fork the reaping thread, if it's not already started

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
