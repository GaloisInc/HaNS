module Hans.Layer.Tcp.Connection where

import Hans.Address.IP4
import Hans.Message.Tcp

import Control.Monad (mzero)
import qualified Data.Map as Map


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

type ListenConnections = Map.Map TcpPort ListenConnection

-- | An empty set of listening connections.
emptyListenConnections :: ListenConnections
emptyListenConnections  = Map.empty

-- | Lookup listening connection information.
lookupListeningConnection :: IP4 -> TcpPort -> ListenConnections
                          -> Maybe ListenConnection
lookupListeningConnection local port cons = do
  lc <- Map.lookup port cons
  case lcHost lc of
    Just addr | addr == local -> return lc
              | otherwise     -> mzero
    Nothing                   -> return lc

-- | Register a listening connection on a port.
addListenConnection :: TcpPort -> ListenConnection
                    -> ListenConnections -> ListenConnections
addListenConnection  = Map.insert

data ListenConnection = ListenConnection
  { lcHost :: Maybe IP4
  } deriving (Show,Eq,Ord)
