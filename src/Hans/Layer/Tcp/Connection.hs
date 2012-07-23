module Hans.Layer.Tcp.Connection where

import Hans.Layer.Tcp.Types

import qualified Data.Map as Map


-- Connections -----------------------------------------------------------------

type Connections = Map.Map SocketId TcpSocket

emptyConnections :: Connections
emptyConnections  = Map.empty

addConnection :: SocketId -> TcpSocket -> Connections -> Connections
addConnection  = Map.insert

lookupConnection :: SocketId -> Connections -> Maybe TcpSocket
lookupConnection = Map.lookup
