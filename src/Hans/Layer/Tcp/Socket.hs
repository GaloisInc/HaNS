module Hans.Layer.Tcp.Socket (
    Socket()
  , sockRemoteHost
  , sockRemotePort
  , sockLocalPort
  , listen
  , accept
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Connection
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Message.Tcp

import Control.Concurrent (MVar,newEmptyMVar,takeMVar,putMVar)


-- Socket Interface ------------------------------------------------------------

data Socket = Socket
  { sockHandle :: TcpHandle
  , sockId     :: !SocketId
  }

sockRemoteHost :: Socket -> IP4
sockRemoteHost  = sidRemoteHost . sockId

sockRemotePort :: Socket -> TcpPort
sockRemotePort  = sidRemotePort . sockId

sockLocalPort :: Socket -> TcpPort
sockLocalPort  = sidLocalPort . sockId

blockResult :: TcpHandle -> (MVar (SocketResult a) -> Tcp ()) -> IO a
blockResult tcp action = do
  res     <- newEmptyMVar
  send tcp (action res)
  sockRes <- takeMVar res
  case sockRes of
    SocketResult a -> return a
    -- XXX throw real exceptions instead of fail
    SocketError    -> fail "SocketError"

listen :: TcpHandle -> IP4 -> TcpPort -> IO Socket
listen tcp _src port = blockResult tcp $ \ res -> do
  ls <- getConnections
  let sid = listenSocketId port
  case lookupConnection sid ls of

    Nothing -> do
      let con = emptyTcpSocket { tcpState = Listen }
      setConnections (addConnection sid con ls)
      output $ putMVar res $ SocketResult Socket
        { sockHandle = tcp
        , sockId     = sid
        }

    Just _ -> output (putMVar res SocketError)

accept :: Socket -> IO Socket
accept sock = blockResult (sockHandle sock) $ \ res -> do
  ls <- getConnections
  case lookupConnection (sockId sock) ls of

    Just con | tcpState con == Listen -> do
      let k sid = putMVar res $ SocketResult $ Socket
            { sockHandle = sockHandle sock
            , sockId     = sid
            }
      setConnections (addConnection (sockId sock) (pushAcceptor k con) ls)

    -- XXX need more descriptive errors
    _ -> output (putMVar res SocketError)
