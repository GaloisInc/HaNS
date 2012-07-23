module Hans.Layer.Tcp.Socket (
    Socket()
  , sockRemoteHost
  , sockRemotePort
  , sockLocalPort
  , listen
  , accept
  , close
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Handlers
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Message.Tcp

import Control.Concurrent (MVar,newEmptyMVar,takeMVar,putMVar)
import Control.Monad (mplus)
import qualified Data.ByteString.Lazy as L


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
  -- XXX put a more meaningful error here
  let unblock = output (putMVar res SocketError)
  send tcp (action res `mplus` unblock)
  sockRes <- takeMVar res
  case sockRes of
    SocketResult a -> return a
    -- XXX throw real exceptions instead of fail
    SocketError    -> fail "SocketError"

listen :: TcpHandle -> IP4 -> TcpPort -> IO Socket
listen tcp _src port = blockResult tcp $ \ res -> do
  let sid = listenSocketId port
  mb <- lookupConnection sid
  case mb of

    Nothing -> do
      let con = emptyTcpSocket { tcpState = Listen }
      addConnection sid con
      output $ putMVar res $ SocketResult Socket
        { sockHandle = tcp
        , sockId     = sid
        }

    Just _ -> output (putMVar res SocketError)

accept :: Socket -> IO Socket
accept sock = blockResult (sockHandle sock) $ \ res -> do
  mb <- lookupConnection (sockId sock)
  case mb of

    Just con | tcpState con == Listen -> do
      let k sid = putMVar res $ SocketResult $ Socket
            { sockHandle = sockHandle sock
            , sockId     = sid
            }
      addConnection (sockId sock) (pushAcceptor k con)

    -- XXX need more descriptive errors
    _ -> output (putMVar res SocketError)


-- | Close an open socket.
close :: Socket -> IO ()
close sock = blockResult (sockHandle sock) $ \ res -> do
  let unblock = putMVar res (SocketResult ())
  mb <- lookupConnection (sockId sock)
  case mb of

    Just tcp -> case tcpState tcp of

      Listen -> do
        remConnection (sockId sock)
        output unblock

      Established -> do
        closeFin tcp
        modifyConnection (sockId sock)
          (pushClose unblock . setConnState FinWait1)

      _ -> output unblock


    -- nothing to do, the socket doesn't exist.
    Nothing -> output unblock


-- Messages --------------------------------------------------------------------

closeFin :: TcpSocket -> Tcp ()
closeFin tcp =
  sendSegment (sidRemoteHost (tcpSocketId tcp)) (mkCloseFin tcp) L.empty
