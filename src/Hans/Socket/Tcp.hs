{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hans.Socket.Tcp where

import           Hans.Addr
import qualified Hans.Buffer.Stream as Stream
import qualified Hans.HashTable as HT
import           Hans.Lens (view,to)
import           Hans.Network
import           Hans.Socket.Types
import           Hans.Tcp.Tcb
import           Hans.Tcp.Message
import           Hans.Tcp.Output
import           Hans.Types

import           Control.Concurrent (newEmptyMVar,tryPutMVar,takeMVar)
import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as L
import           Data.IORef (readIORef)


-- TCP Sockets -----------------------------------------------------------------

data TcpSocket addr = TcpSocket { tcpNS  :: !NetworkStack
                                , tcpTcb :: !Tcb
                                }

instance HasNetworkStack (TcpSocket addr) where
  networkStack = to tcpNS

-- | Add a new active connection to the TCP state. The connection will initially
-- be in the 'SynSent' state, as a Syn will be sent when the 'Tcb' is created.
activeOpen :: Network addr
           => NetworkStack -> RouteInfo addr -> SockPort -> addr -> SockPort
           -> IO Tcb
activeOpen ns ri srcPort dst dstPort =
  do let ri'  = toAddr `fmap` ri
         dst' = toAddr dst

     done <- newEmptyMVar

     iss <- nextISS (view tcpState ns) (riSource ri') srcPort dst' dstPort
     tcb <- newTcb ns Nothing iss ri' srcPort dst' dstPort Closed
                (\_ -> tryPutMVar done True  >> return ())
                (\_ -> tryPutMVar done False >> return ())

     let key            = Key dst' dstPort (riSource ri') srcPort
         update Nothing = (Just tcb, True)
         update Just{}  = (Nothing, False)
     success <- HT.alter update key (view tcpActive ns)
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


data TcpListenSocket addr = TcpListenSocket { tlNS :: !NetworkStack
                                            }


instance Socket TcpListenSocket where

  sClose sock = undefined


instance ListenSocket TcpListenSocket where

  type Client TcpListenSocket = TcpSocket

  sListen ns src srcPort = undefined

  sAccept sock = undefined




