module Hans.Layer.Tcp.Connection where

import Hans.Address.IP4
import Hans.Message.Tcp

import qualified Data.Map as Map
import qualified Data.Set as Set


-- Connections -----------------------------------------------------------------

type Connections = Map.Map ConnIdent Connection

emptyConnections :: Connections
emptyConnections  = Map.empty

lookupConnection :: ConnIdent -> Connections -> Maybe Connection
lookupConnection  = Map.lookup

addConnection :: ConnIdent -> Connection -> Connections -> Connections
addConnection  = Map.insert

-- | Connection identifiers.  This is the pair of both host addresses, and both
-- tcp ports.  In the future, this could have the address abstracted out,
-- allowing for the implementation to bet network layer agnostic.  For now, it's
-- got IP4 hard-wired.
data ConnIdent = ConnIdent
  { ciRemoteHost :: !IP4
  , ciRemotePort :: !TcpPort
  , ciLocalHost  :: !IP4
  , ciLocalPort  :: !TcpPort
  } deriving (Show,Eq,Ord)

incomingConnIdent :: IP4 -> IP4 -> TcpHeader -> ConnIdent
incomingConnIdent remote local hdr = ConnIdent
  { ciRemoteHost = remote
  , ciRemotePort = tcpSourcePort hdr
  , ciLocalHost  = local
  , ciLocalPort  = tcpDestPort hdr
  }

data Connection = Connection
  { conState :: ConnectionState
  } deriving (Show)

emptyConnection :: ConnectionState -> Connection
emptyConnection state = Connection
  { conState = state
  }

-- | TCP connection states.
data ConnectionState
  = Closed
  | Closing
  | CloseWait
  | Listen
  | SynRcvd
  | SynSent
  | FinWait1
  | FinWait2
  | TimeWait
  | Established
  | LastAck
    deriving (Show)


-- Listening Connections -------------------------------------------------------

type ListenConnections = Set.Set ListenConnection

emptyListenConnections :: ListenConnections
emptyListenConnections  = Set.empty

isListening :: ListenConnection -> ListenConnections -> Bool
isListening  = Set.member

addListenConnection :: ListenConnection -> ListenConnections -> ListenConnections
addListenConnection  = Set.insert

data ListenConnection = ListenConnection
  { lcHost :: !IP4
  , lcPort :: !TcpPort
  } deriving (Show,Eq,Ord)
