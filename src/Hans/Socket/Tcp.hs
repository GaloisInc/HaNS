{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hans.Socket.Tcp where

import           Hans.Addr
import qualified Hans.Buffer.Stream as Stream
import qualified Hans.HashTable as HT
import           Hans.Lens (Getting,view,to)
import           Hans.Network
import           Hans.Socket.Types
import           Hans.Tcp.Tcb
import           Hans.Tcp.Message
import           Hans.Tcp.Output
import qualified Hans.Tcp.SendWindow as Send
import           Hans.Types

import           Control.Concurrent (newEmptyMVar,tryPutMVar,takeMVar)
import           Control.Exception (throwIO)
import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as L
import           Data.IORef (readIORef)
import           Data.Time.Clock (getCurrentTime)
import           System.CPUTime (getCPUTime)


-- TCP Sockets -----------------------------------------------------------------

data TcpSocket addr = TcpSocket { tcpNS  :: !NetworkStack
                                , tcpTcb :: !Tcb
                                }

instance HasNetworkStack (TcpSocket addr) where
  networkStack = to tcpNS

-- | Routing information for this socket.
tcpRoute :: NetworkAddr addr => Getting r (TcpSocket addr) (RouteInfo addr)
tcpRoute  = to (\ TcpSocket { tcpTcb = Tcb { .. } } -> cast tcbRouteInfo)
  where
  cast RouteInfo { .. } =
    case (fromAddr riSource, fromAddr riNext) of
      (Just a,Just b) -> RouteInfo { riSource = a, riNext = b, .. }
      _               -> error "tcpRoute: invalid address combination"

-- | The source address of this socket.
tcpLocalAddr :: NetworkAddr addr => Getting r (TcpSocket addr) addr
tcpLocalAddr  = tcpRoute . to riSource

-- | The local port for this socket.
tcpLocalPort :: Getting r (TcpSocket addr) SockPort
tcpLocalPort  = to (\ TcpSocket { tcpTcb = Tcb { .. } } -> tcbLocalPort )

-- | The remote address of this socket.
tcpRemoteAddr :: NetworkAddr addr => Getting r (TcpSocket addr) addr
tcpRemoteAddr  = to (\ TcpSocket { tcpTcb = Tcb { .. } } -> cast tcbRemote)
  where
  cast addr =
    case fromAddr addr of
      Just a  -> a
      Nothing -> error "tcpRemoteHost: invalid remote address"

-- | The remote port of this socket.
tcpRemotePort :: Getting r (TcpSocket addr) SockPort
tcpRemotePort  = to (\ TcpSocket { tcpTcb = Tcb { .. } } -> tcbRemotePort)


-- | Add a new active connection to the TCP state. The connection will initially
-- be in the 'SynSent' state, as a Syn will be sent when the 'Tcb' is created.
activeOpen :: Network addr
           => NetworkStack -> RouteInfo addr -> SockPort -> addr -> SockPort
           -> IO Tcb
activeOpen ns ri srcPort dst dstPort =
  do let ri'  = toAddr `fmap` ri
         dst' = toAddr dst

     done <- newEmptyMVar

     now   <- getCurrentTime
     tsval <- getCPUTime
     let tsc = Send.initialTSClock (fromInteger tsval) now

     iss <- nextIss (view tcpState ns) (riSource ri') srcPort dst' dstPort
     tcb <- newTcb ns Nothing iss ri' srcPort dst' dstPort Closed tsc
                (\_ _ -> tryPutMVar done True  >> return ())
                (\_ _ -> tryPutMVar done False >> return ())

     let update Nothing = (Just tcb, True)
         update Just{}  = (Nothing, False)
     success <- HT.alter update (view tcbKey tcb) (view tcpActive ns)
     if success
        then
          do syn <- mkSyn tcb
             _   <- sendWithTcb ns tcb syn L.empty
             setState tcb SynSent

             established <- takeMVar done
             if established
                then return tcb
                else throwIO ConnectionRefused

        else throwIO AlreadyConnected


instance Socket TcpSocket where

  sClose TcpSocket { .. } =
    do state <- readIORef (tcbState tcpTcb)
       case state of

         -- the remote side closed the connection, so we just need to cleanup.
         CloseWait ->
           do sendFin tcpNS tcpTcb
              setState tcpTcb LastAck

         Established ->
           do sendFin tcpNS tcpTcb
              setState tcpTcb FinWait1

         -- SynSent, Listen, Closed, Closing, LastAck
         _ -> return ()


-- | Guard an action that will use the send window.
guardSend :: Tcb -> IO r -> IO r
guardSend tcb send =
  do st <- getState tcb
     case st of
       Closed -> throwIO NoConnection

       Listen -> error "guardSend: Listen state for active tcb"

       -- the send window should queue these
       SynReceived -> send
       SynSent     -> send

       Established -> send
       CloseWait   -> send

       -- FinWait1, FinWait2, Closing, LastAck, TimeWait
       _ -> throwIO ConnectionClosing


-- | Guard an action that will use the receive buffer.
guardRecv :: Tcb -> IO r -> IO r
guardRecv tcb recv =
  do st <- getState tcb
     case st of
       Closed      -> throwIO NoConnection

       -- these three cases will block until data is available
       Listen      -> recv
       SynSent     -> recv
       SynReceived -> recv

       -- the common case
       Established -> recv
       FinWait1    -> recv
       FinWait2    -> recv

       -- XXX: is this enough?
       CloseWait   -> do avail <- Stream.bytesAvailable (tcbRecvBuffer tcb)
                         if avail
                            then recv
                            else throwIO ConnectionClosing

       Closing     -> throwIO ConnectionClosing
       LastAck     -> throwIO ConnectionClosing
       TimeWait    -> throwIO ConnectionClosing



instance DataSocket TcpSocket where

  sConnect ns SocketConfig { .. } mbDev src mbSrcPort dst dstPort =
    do let tcpNS = view networkStack ns

       ri <- route tcpNS mbDev src dst

       srcPort <- case mbSrcPort of
                    Just port -> return port
                    Nothing   ->
                      do mb <- nextTcpPort tcpNS (toAddr src) (toAddr dst) dstPort
                         case mb of
                           Just port -> return port
                           Nothing   -> throwIO NoPortAvailable

       -- activeOpen will start the connection for us, sending a SYN to the
       -- remote end of the connection.
       tcpTcb <- activeOpen tcpNS ri srcPort dst dstPort
       return TcpSocket { .. }

  -- segmentize the bytes, and return to the user the number of bytes that have
  -- been moved into the send window
  sWrite TcpSocket { .. } bytes =
    guardSend tcpTcb $ do len <- sendData tcpNS tcpTcb bytes
                          return $! fromIntegral len

  sRead    TcpSocket { .. } len = guardRecv tcpTcb (receiveBytes    len tcpTcb)
  sTryRead TcpSocket { .. } len = guardRecv tcpTcb (tryReceiveBytes len tcpTcb)


data TcpListenSocket addr = TcpListenSocket { tlNS  :: !NetworkStack
                                            , tlTcb :: !ListenTcb
                                            }


instance Socket TcpListenSocket where

  -- NOTE: as listen sockets are always in the Listen state, we don't need to
  -- consider any of the other cases on page 60 of RFC 793.
  sClose TcpListenSocket { .. } = deleteListening tlNS tlTcb
  {-# INLINE sClose #-}


instance ListenSocket TcpListenSocket where

  type Client TcpListenSocket = TcpSocket

  sListen ns SocketConfig { .. } src srcPort backlog =
    do let tlNS = view networkStack ns
       tlTcb <- newListenTcb (toAddr src) srcPort backlog

       created <- registerListening tlNS tlTcb
       unless created (throwIO AlreadyListening)

       return $! TcpListenSocket { .. }

  sAccept TcpListenSocket { .. } =
    do tcpTcb <- acceptTcb tlTcb
       return $! TcpSocket { tcpNS = tlNS, .. }
