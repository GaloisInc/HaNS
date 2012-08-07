{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Layer.Tcp.Socket (
    Socket()
  , sockRemoteHost
  , sockRemotePort
  , sockLocalPort
  , listen
  , accept
  , close
  , sendBytes
  , recvBytes
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Handlers
import Hans.Layer.Tcp.Messages
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp

import Control.Concurrent (MVar,newEmptyMVar,takeMVar,putMVar)
import Control.Exception (Exception,throwIO)
import Control.Monad (mplus)
import Data.Int (Int64)
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
--
-- XXX closing the socket should also unblock any other threads waiting on
-- socket actions
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
      let con = (emptyTcpSocket 0 0) { tcpState = Listen }
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
accept sock = blockResult (sockHandle sock) $ \ res ->
  establishedConnection (sockId sock) $ do
    state <- getState
    case state of
      Listen -> pushAcceptor $ \ sid -> putMVar res $ SocketResult $ Socket
        { sockHandle = sockHandle sock
        , sockId     = sid
        }

      -- XXX need more descriptive errors
      _ -> outputS (putMVar res (socketError AcceptError))


-- Close -----------------------------------------------------------------------

data CloseError = CloseError
    deriving (Show,Typeable)

instance Exception CloseError

-- | Close an open socket.
close :: Socket -> IO ()
close sock = blockResult (sockHandle sock) $ \ res -> do
  let unblock   = output . putMVar res
      connected = establishedConnection (sockId sock) $ do
        userClose
        state <- getState
        case state of

          Established -> do
            finAck
            setState FinWait1

          -- XXX how should we close a listening socket?
          Listen -> do
            setState Closed

          _ -> return ()

        return (SocketResult ())

  -- closing a connection that doesn't exist causes a CloseError
  unblock =<< connected `mplus` return (socketError CloseError)

userClose :: Sock ()
userClose  = modifyTcpSocket_ (\tcp -> tcp { tcpUserClosed = True })


-- Writing ---------------------------------------------------------------------

-- | Send bytes over a socket.  The number of bytes delivered will be returned,
-- with 0 representing the other side having closed the connection.
sendBytes :: Socket -> L.ByteString -> IO Int64
sendBytes sock bytes = blockResult (sockHandle sock) performSend
  where
  performSend res = establishedConnection (sockId sock) $ do
    let result len    = putMVar res (SocketResult len)
    let wakeup continue
          | continue  = send (sockHandle sock) (performSend res)
          | otherwise = result 0
    mbWritten <- modifyTcpSocket $ \ tcp ->
      let (mbWritten,bufOut) = writeBytes bytes wakeup (tcpOutBuffer tcp)
       in (mbWritten,tcp { tcpOutBuffer = bufOut })
    case mbWritten of
      Just len -> outputS (result len)
      Nothing  -> return ()
    outputSegments

-- Reading ---------------------------------------------------------------------

-- | Receive bytes from a socket.  A null ByteString represents the other end
-- closing the socket.
recvBytes :: Socket -> Int64 -> IO L.ByteString
recvBytes sock len = blockResult (sockHandle sock) performRecv
  where
  performRecv res = establishedConnection (sockId sock) $ do
    let result bytes  = putMVar res (SocketResult bytes)
        wakeup continue
          | continue  = send (sockHandle sock) (performRecv res)
          | otherwise = result L.empty
    mbRead <- modifyTcpSocket $ \ tcp ->
      let (mbRead,bufIn) = readBytes len wakeup (tcpInBuffer tcp)
       in (mbRead,tcp { tcpInBuffer = bufIn })
    case mbRead of
      Just bytes -> outputS (result bytes)
      Nothing    -> return ()
