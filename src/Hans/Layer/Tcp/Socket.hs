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
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4 (Mtu)
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
  let unblock = output . putMVar res
      established = establishedConnection (sockId sock) $ do
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
  unblock =<< established `mplus` return (socketError CloseError)

userClose :: Sock ()
userClose  = modifyTcpSocket_ (\tcp -> tcp { tcpUserClosed = True })


-- Writing ---------------------------------------------------------------------

-- | Send bytes over a socket.  The operation returns once the bytes have been
-- confirmed delivered.
sendBytes :: Socket -> L.ByteString -> IO ()
sendBytes sock bytes = blockResult (sockHandle sock) $ \ res ->
  establishedConnection (sockId sock) $ do
    let final = putMVar res (SocketResult ())
        -- XXX where should we get the MTU from?
        mtu   = 8 -- 1400
    mapM_ emitSegment =<< modifyTcpSocket (bytesToSegments mtu bytes final)

-- | Emit a segment to the other end.
emitSegment :: Segment -> Sock ()
emitSegment seg = do
  tcpOutput (segHeader seg) (segBody seg)
  modifyTcpSocket_ (\ tcp -> tcp { tcpOut = waitForAck seg (tcpOut tcp) })

-- | Wrap up the bytestring into a number of segments.
--
-- XXX start paying attention to the available window
-- XXX this should probably know about the MTU, how should this be communicated?
bytesToSegments :: Mtu -> L.ByteString -> Finalizer -> TcpSocket
                -> ([Segment],TcpSocket)
bytesToSegments mtu bytes0 final sock0 = loop bytes0 sock0
  where
  mtu'          = fromIntegral mtu
  loop bytes sock
    | L.null r  = ([seg { segFinalizer = Just final }],sock')
    | otherwise = (seg:rest,sockFinal)
    where
    (body,r) = L.splitAt mtu' bytes

    seg = Segment
      { segHeader    = mkData sock
      , segBody      = body
      , segFinalizer = Nothing
      }

    sock' = sock { tcpSndNxt = tcpSndNxt sock + fromIntegral (L.length body) }

    (rest,sockFinal) = loop r sock'
