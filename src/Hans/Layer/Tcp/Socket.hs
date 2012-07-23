{-# LANGUAGE DeriveDataTypeable #-}

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
import Control.Exception (Exception,throwIO)
import Control.Monad (mplus)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as L


-- Socket Interface ------------------------------------------------------------

data Socket = Socket
  { sockHandle :: TcpHandle
  , sockId     :: !SocketId
  }

-- | The remote host of a socket.
sockRemoteHost :: Socket -> IP4
sockRemoteHost  = sidRemoteHost . sockId

-- | The remote port of a socket.
sockRemotePort :: Socket -> TcpPort
sockRemotePort  = sidRemotePort . sockId

-- | The local port of a socket.
sockLocalPort :: Socket -> TcpPort
sockLocalPort  = sidLocalPort . sockId

data SocketGenericError = SocketGenericError
    deriving (Show,Typeable)

instance Exception SocketGenericError

-- | Block on the result of a Tcp action, from a different context.
blockResult :: TcpHandle -> (MVar (SocketResult a) -> Tcp ()) -> IO a
blockResult tcp action = do
  res     <- newEmptyMVar
  -- XXX put a more meaningful error here
  let unblock = output (putMVar res (socketError SocketGenericError))
  send tcp (action res `mplus` unblock)
  sockRes <- takeMVar res
  case sockRes of
    SocketResult a -> return a
    SocketError e  -> throwIO e


-- Listen ----------------------------------------------------------------------

data ListenError = ListenError
    deriving (Show,Typeable)

instance Exception ListenError

-- | Open a new listening socket that can be used to accept new connections.
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

    Just _ -> output (putMVar res (socketError ListenError))


-- Accept ----------------------------------------------------------------------

data AcceptError = AcceptError
    deriving (Show,Typeable)

instance Exception AcceptError

-- | Accept new incoming connections on a listening socket.
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
    _ -> output (putMVar res (socketError AcceptError))


-- Close -----------------------------------------------------------------------

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
    -- XXX should this throw an error?
    Nothing -> output unblock

-- | Send a FIN packet to begin closing a connection.
closeFin :: TcpSocket -> Tcp ()
closeFin tcp =
  sendSegment (sidRemoteHost (tcpSocketId tcp)) (mkCloseFin tcp) L.empty
