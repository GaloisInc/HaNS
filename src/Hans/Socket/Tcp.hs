{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hans.Socket.Tcp where

import           Hans.Addr
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
           -> IO (Maybe Tcb)
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
                then return (Just tcb)
                else return Nothing

        else return Nothing


instance Socket TcpSocket where

  sClose TcpSocket { .. } =
    do st <- readIORef (tcbState tcpTcb)
       case st of
         Closed -> throwIO DoesNotExist

         -- impossible for a data socket
         Listen -> fail "sClose(TcpSocket): socket was in Listen"


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
       mbTcb <- activeOpen tcpNS ri srcPort dst dstPort
       case mbTcb of
         -- XXX need to wait until the connection is finalized
         Just tcpTcb -> return TcpSocket { .. }
         Nothing     -> throwIO AlreadyConnected

  sWrite sock bytes = undefined

  sRead sock len = undefined

  sTryRead ns len = undefined


data TcpListenSocket addr = TcpListenSocket { tlNS :: !NetworkStack
                                            }


instance Socket TcpListenSocket where

  sClose sock = undefined


instance ListenSocket TcpListenSocket where

  type Client TcpListenSocket = TcpSocket

  sListen ns src srcPort = undefined

  sAccept sock = undefined




