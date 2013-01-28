{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Layer.Tcp.Socket (
    -- * Socket Layer
    Socket()
  , SocketError(..)
  , listenPort
  , acceptSocket
  , connect
  , sendSocket
  , recvSocket
  , closeSocket
  , readBytes
  , readLine

  , getSocketHost
  , getSocketPort
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Monad
import Hans.Message.Tcp (TcpPort(..))

import Network.TCP.LTS.User (tcp_process_user_request)
import Network.TCP.Type.Base
    (IPAddr(..),SocketID(..),TCPAddr(..))
import Network.TCP.Type.Syscall (SockReq(..),SockRsp(..))

import Control.Exception (throwIO,Exception)
import Control.Concurrent (MVar,newMVar,newEmptyMVar,takeMVar,putMVar)
import Data.Typeable (Typeable)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

-- Socket Layer ----------------------------------------------------------------

data Socket = Socket
  { socketTcpHandle :: TcpHandle
  , socketId        :: !SocketID
  , socketBuffer    :: MVar L.ByteString
  , socketPort      :: !TcpPort
  , socketHost      :: !IP4
  }

mkSocket :: TcpHandle -> SocketID -> MVar L.ByteString -> Socket
mkSocket tcp sid@(SocketID (_,TCPAddr (IPAddr a,p))) buf = Socket
  { socketTcpHandle = tcp
  , socketId        = sid
  , socketBuffer    = buf
  , socketPort      = port
  , socketHost      = addr
  }
  where
  port = TcpPort p
  addr = convertFromWord32 a

getSocketHost :: Socket -> IP4
getSocketHost  = socketHost

getSocketPort :: Socket -> TcpPort
getSocketPort  = socketPort

data SocketResult a
  = SocketResult a
  | SocketError SocketError

data SocketError
  = ListenError String
  | AcceptError String
  | ConnectError String
  | SendError String
  | RecvError String
  | CloseError String
    deriving (Typeable,Show)

instance Exception SocketError

-- | Block on a socket operation, waiting for the TCP layer to finish an action.
blockResult :: TcpHandle -> (MVar (SocketResult a) -> Tcp ()) -> IO a
blockResult tcp k = do
  var <- newEmptyMVar
  send tcp (k var)
  sr  <- takeMVar var
  case sr of
    SocketResult a -> return a
    SocketError se -> throwIO se

-- | Call @output@ if the @Tcp@ action returns a @Just@.
maybeOutput :: Tcp (Maybe (IO ())) -> Tcp ()
maybeOutput body = do
  mb <- body
  case mb of
    Just m  -> output m
    Nothing -> return ()

-- | Listen on a port.
listenPort :: TcpHandle -> TcpPort -> IO Socket
listenPort tcp (TcpPort port) = blockResult tcp $ \ res -> do
  let mkError = SocketError . ListenError
      k rsp = case rsp of
        SockNew sid   -> do
          buf <- newMVar L.empty
          putMVar res (SocketResult (mkSocket tcp sid buf))
        SockError err -> putMVar res (mkError err)
        _             -> putMVar res (mkError "Unexpected response")
  maybeOutput (tcp_process_user_request (SockListen port,k))

-- | Accept a client connection on a @Socket@.
acceptSocket :: Socket -> IO Socket
acceptSocket sock = blockResult (socketTcpHandle sock) $ \ res -> do
  let mkError = SocketError . AcceptError
      k rsp = case rsp of
        SockNew sid   -> do
          buf <- newMVar L.empty
          putMVar res (SocketResult (mkSocket (socketTcpHandle sock) sid buf))
        SockError err -> putMVar res (mkError err)
        _             -> putMVar res (mkError "Unexpected response")
  maybeOutput (tcp_process_user_request (SockAccept (socketId sock),k))

-- | Connect to a remote server.
connect :: TcpHandle -> IP4 -> IP4 -> TcpPort -> IO Socket
connect tcp src dst (TcpPort port) = blockResult tcp $ \ res -> do
  let us   = IPAddr (convertToWord32 src)
      them = TCPAddr (IPAddr (convertToWord32 dst), port)
      mkError = SocketError . ConnectError
      k rsp = case rsp of
        SockNew sid   -> do
          buf <- newMVar L.empty
          putMVar res (SocketResult (mkSocket tcp sid buf))
        SockError err -> putMVar res (mkError err)
        _             -> putMVar res (mkError "Unexpected response")
  maybeOutput (tcp_process_user_request (SockConnect us them,k))

-- | Send on a @Socket@.
sendSocket :: Socket -> S.ByteString -> IO ()
sendSocket sock bytes = blockResult (socketTcpHandle sock) $ \ res -> do
  let mkError = SocketError . SendError
      k rsp = putMVar res $! case rsp of
        SockOK        -> SocketResult ()
        SockError err -> mkError err
        _             -> mkError "Unexpected response"
  maybeOutput (tcp_process_user_request (SockSend (socketId sock) bytes,k))

-- | Receive from a @Socket@.
recvSocket :: Socket -> IO S.ByteString
recvSocket sock = blockResult (socketTcpHandle sock) $ \ res -> do
  let mkError = SocketError . RecvError
      k rsp = putMVar res $! case rsp of
        SockData bs   -> SocketResult bs
        SockError err -> mkError err
        _             -> mkError "Unexpected response"
  maybeOutput (tcp_process_user_request (SockRecv (socketId sock),k))

-- | Close a socket.
closeSocket :: Socket -> IO ()
closeSocket sock =
  blockResult (socketTcpHandle sock) $ \ res -> do
  let mkError = SocketError . CloseError
      k rsp = putMVar res $! case rsp of
        SockOK        -> SocketResult ()
        SockError err -> mkError err
        _             -> mkError "Unexpected response"
  maybeOutput (tcp_process_user_request (SockClose (socketId sock),k))


-- Derived Interaction ---------------------------------------------------------

-- | Read n bytes from a @Socket@.
readBytes :: Socket -> Int -> IO S.ByteString
readBytes sock goal = do
  buf <- takeMVar (socketBuffer sock)
  loop buf (fromIntegral (L.length buf))
  where
  loop buf len
    | goal <= len = finish buf
    | otherwise   = do
      bytes <- recvSocket sock
      if S.null bytes
         then finish buf
         else loop (buf `L.append` L.fromChunks [bytes]) (len + S.length bytes)

  finish buf = do
    let (as,bs) = L.splitAt (fromIntegral goal) buf
    putMVar (socketBuffer sock) bs
    return (S.concat (L.toChunks as))

-- | Read until a CRLF, LF or CR are read.
readLine :: Socket -> IO S.ByteString
readLine sock = do
  buf <- takeMVar (socketBuffer sock)
  loop False 0 buf
  where
  loop cr ix buf
    | L.length buf <= ix = fillBuffer cr ix buf
    | otherwise  =
      case L.index buf ix of
        0x0d          -> loop True (ix+1) buf
        0x0a          -> finish (ix+1) buf
        _ | cr        -> finish ix buf
          | otherwise -> loop False (ix+1) buf

  fillBuffer cr ix buf = do
    bytes <- recvSocket sock
    if S.null bytes
       then finish ix buf
       else loop cr ix (buf `L.append` L.fromChunks [bytes])

  finish ix buf = do
    let (as,bs) = L.splitAt ix buf
    putMVar (socketBuffer sock) bs
    return (S.concat (L.toChunks as))
