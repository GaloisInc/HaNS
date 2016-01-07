{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Hans.Socket (
    -- * Abstract Sockets
    Socket(..),
    ListenSocket(..),
    DataSocket(..),
    SocketConfig(..), defaultSocketConfig,
    SockPort,

    -- ** UDP Sockets
    UdpSocket(),
    newUdpSocket,
    sendto,
    recvfrom,

    -- ** TCP Sockets
    TcpSocket(), TcpListenSocket(),

  ) where

import           Hans.Addr
import qualified Hans.Buffer.Datagram as DGram
import           Hans.Device.Types (Device)
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Network
import           Hans.Types
                     (HasNetworkStack(..),NetworkStack,registerRecv,UdpBuffer
                     ,nextUdpPort,nextTcpPort)
import           Hans.Udp.Input ()
import           Hans.Udp.Output (primSendUdp)
import qualified Hans.Tcp.Message as Tcp
import qualified Hans.Tcp.Output as Tcp
import qualified Hans.Tcp.State as Tcp
import qualified Hans.Tcp.Tcb as Tcp

import           Control.Concurrent (newEmptyMVar,tryPutMVar,takeMVar)
import           Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,readIORef)
import           Data.Typeable (Typeable)
import           Data.Word (Word16)


-- Socket Addresses ------------------------------------------------------------

type SockPort = Word16


-- Generic Socket Operations ---------------------------------------------------

data SocketConfig = SocketConfig { scRecvBufferSize :: !Int
                                   -- ^ Bytes to buffer
                                 } deriving (Show)

defaultSocketConfig :: SocketConfig
defaultSocketConfig  = SocketConfig { scRecvBufferSize = 4096 }

class Socket sock where

  -- | Close an open socket.
  sClose :: Network addr => sock addr -> IO ()


class (DataSocket (Client sock), Socket sock) => ListenSocket sock where

  type Client sock :: * -> *

  -- | Create a listening socket, with a backlog of n.
  sListen :: (HasNetworkStack ns, Network addr)
          => ns -> SocketConfig -> addr -> SockPort -> Int -> IO (sock addr)

  sAccept :: Network addr => sock addr -> IO (Client sock addr)


class Socket sock => DataSocket sock where

  -- | Connect this socket to one on a remote machine.
  sConnect :: (HasNetworkStack ns, Network addr)
           => ns
           -> SocketConfig
           -> Maybe Device
           -> addr           -- ^ Local address
           -> Maybe SockPort -- ^ Local port
           -> addr           -- ^ Remote host
           -> SockPort       -- ^ Remote port
           -> IO (sock addr)

  -- | Send a chunk of data on a socket.
  sWrite :: Network addr => sock addr -> L.ByteString -> IO Int

  -- | Read a chunk of data from a socket. Reading an empty result indicates
  -- that the socket has closed.
  sRead :: Network addr => sock addr -> Int -> IO L.ByteString

  -- | Non-blocking read from a socket. Reading an empty result means that the
  -- socket has closed, while reading a 'Nothing' result indicates that there
  -- was no data available.
  sTryRead :: Network addr => sock addr -> Int -> IO (Maybe L.ByteString)


-- Exceptions ------------------------------------------------------------------

data SocketException addr = AlreadyConnected !addr !SockPort !addr !SockPort
                            -- ^ This connection already exists.

                          | AlreadyListening !addr !SockPort
                            -- ^ Something is already listening on this
                            -- host/port combination.

                          | NoRouteToHost
                            -- ^ It's not possible to reach this host from this
                            -- source address.

                          | NoConnection
                            -- ^ No information about the other end of the
                            -- socket was present.

                          | NoPortAvailable
                            -- ^ All ports are in use.
                            deriving (Show,Typeable)

instance (Show addr, Typeable addr) => Exception (SocketException addr)

throwSE :: (Show addr, Typeable addr) => addr -> SocketException addr -> IO a
throwSE _ = throwIO
{-# INLINE throwSE #-}


-- UDP Sockets -----------------------------------------------------------------

data SockState addr = KnownRoute !(RouteInfo addr) !addr !SockPort !SockPort
                      -- ^ Cached route, and port information

                    | KnownSource !(Maybe Device) !addr !SockPort
                      -- ^ Known source only.


data UdpSocket addr = UdpSocket { udpNS        :: !NetworkStack
                                , udpBuffer    :: !UdpBuffer
                                , udpSockState :: !(IORef (SockState addr))
                                , udpClose     :: !(IO ())
                                }

instance HasNetworkStack (UdpSocket addr) where
  networkStack = to udpNS
  {-# INLINE networkStack #-}

instance Socket UdpSocket where

  sClose UdpSocket { .. } = udpClose
  {-# INLINE sClose #-}


newUdpSocket :: (HasNetworkStack ns, Network addr)
             => ns
             -> SocketConfig
             -> Maybe Device
             -> addr
             -> Maybe SockPort
             -> IO (UdpSocket addr)

newUdpSocket ns SocketConfig { .. } mbDev src mbSrcPort =
  do let udpNS = view networkStack ns

     srcPort <- case mbSrcPort of
                 Nothing -> do mb <- nextUdpPort udpNS (toAddr src)
                               case mb of
                                 Just port -> return port
                                 Nothing   -> throwSE src NoPortAvailable

                 Just p  -> return p

     udpSockState <- newIORef (KnownSource mbDev src srcPort)

     udpBuffer <- DGram.newBuffer scRecvBufferSize

     -- XXX: Need some SocketError exceptions: this only happens if there's
     -- already something listening
     mbClose  <- registerRecv udpNS (toAddr src) srcPort udpBuffer
     udpClose <- case mbClose of
                   Just unreg -> return unreg
                   Nothing    -> throwIO (AlreadyListening src srcPort)

     return $! UdpSocket { .. }


instance DataSocket UdpSocket where

  -- Always lookup the route before modifying the state of the socket, so that
  -- the state can be manipulated atomically.
  sConnect ns SocketConfig { .. } mbDev src mbPort dst dstPort =
    do let udpNS = view networkStack ns

       ri <- route udpNS mbDev src dst

       srcPort <- case mbPort of
                    Just p  -> return p
                    Nothing -> do mb <- nextUdpPort udpNS (toAddr src)
                                  case mb of
                                    Just port -> return port
                                    Nothing   -> throwSE src NoPortAvailable

       udpSockState <- newIORef (KnownRoute ri dst srcPort dstPort)

       udpBuffer <- DGram.newBuffer scRecvBufferSize

       mbClose  <- registerRecv udpNS (toAddr src) srcPort udpBuffer
       udpClose <- case mbClose of
                     Just unreg -> return unreg
                     Nothing    -> throwIO (AlreadyConnected src srcPort dst dstPort)

       return UdpSocket { .. }

  sWrite UdpSocket { .. } bytes =
    do path <- readIORef udpSockState
       case path of

         KnownRoute ri dst srcPort dstPort ->
           do sent <- primSendUdp udpNS ri dst srcPort dstPort bytes
              if sent
                 then return (fromIntegral (L.length bytes))
                 else return (-1)

         KnownSource _ src _ ->
              throwSE src NoConnection

  sRead UdpSocket { .. } len =
    do (_,buf) <- DGram.readChunk udpBuffer
       return (L.fromStrict (S.take len buf))

  sTryRead UdpSocket { .. } len =
    do mb <- DGram.tryReadChunk udpBuffer
       case mb of
         Just (_,buf) -> return $! Just $! L.fromStrict $! S.take len buf
         Nothing      -> return Nothing



-- | Receive, with information about who sent this datagram.
recvfrom :: Network addr
         => UdpSocket addr -> IO (Device,addr,SockPort,L.ByteString)
recvfrom UdpSocket { .. } = loop
  where

  -- NOTE: this loop shouldn't run more than one time, as it's very unlikely
  -- that we receive a packet destined for a different protocol address
  loop =
    do ((dev,addr,srcPort,_,_), chunk) <- DGram.readChunk udpBuffer
       case fromAddr addr of
         Just src -> return (dev,src,srcPort,L.fromStrict chunk)
         Nothing  -> loop
{-# LANGUAGE recvfrom #-}


-- | Send to a specific end host.
sendto :: Network addr
       => UdpSocket addr -> addr -> SockPort -> L.ByteString -> IO ()
sendto UdpSocket { .. } = \ dst dstPort bytes ->
  do state <- readIORef udpSockState
     case state of

       KnownSource mbDev src srcPort ->
         do mbRoute <- route' udpNS mbDev src dst
            case mbRoute of

              Just ri ->
                  do _ <- primSendUdp udpNS ri dst srcPort dstPort bytes
                     return ()

              -- no route found, but we're broadcasting using a known device
              Nothing
                | Just dev <- mbDev, isBroadcastAddr dst ->
                  do let ri = RouteInfo { riSource = src
                                        , riNext   = dst
                                        , riDev    = dev }
                     _ <- primSendUdp udpNS ri dst srcPort dstPort bytes
                     return ()

              _ ->
                  throwSE dst NoRouteToHost

       -- we can't use sendto if sConnect has been used already
       KnownRoute ri dst' srcPort dstPort' ->
         throwSE dst (AlreadyConnected (riSource ri) srcPort dst' dstPort')
{-# INLINE sendto #-}


-- TCP Sockets -----------------------------------------------------------------

data TcpSocket addr = TcpSocket { tcpNS  :: !NetworkStack
                                , tcpTcb :: !Tcp.Tcb
                                }

-- | Add a new active connection to the TCP state. The connection will initially
-- be in the 'SynSent' state, as a Syn will be sent when the 'Tcb' is created.
activeOpen :: Network addr
           => NetworkStack -> RouteInfo addr -> SockPort -> addr -> SockPort
           -> IO (Maybe Tcp.Tcb)
activeOpen ns ri srcPort dst dstPort =
  do let ri'  = toAddr `fmap` ri
         dst' = toAddr dst

     done <- newEmptyMVar

     iss <- Tcp.nextISS (view Tcp.tcpState ns) (riSource ri') srcPort dst' dstPort
     tcb <- Tcp.newTcb ns Nothing iss ri' srcPort dst' dstPort Tcp.Closed
                (\_ -> tryPutMVar done True  >> return ())
                (\_ -> tryPutMVar done False >> return ())

     let key            = Tcp.Key dst' dstPort (riSource ri') srcPort
         update Nothing = (Just tcb, True)
         update Just{}  = (Nothing, False)
     success <- HT.alter update key (view Tcp.tcpActive ns)
     if success
        then
          do syn <- Tcp.mkSyn tcb
             _   <- Tcp.sendWithTcb ns tcb syn L.empty
             Tcp.setState tcb Tcp.SynSent

             established <- takeMVar done
             if established
                then return (Just tcb)
                else return Nothing

        else return Nothing


instance Socket TcpSocket where

  sClose sock = undefined

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
                           Nothing   -> throwSE dst NoPortAvailable

       -- activeOpen will start the connection for us, sending a SYN to the
       -- remote end of the connection.
       mbTcb <- activeOpen tcpNS ri srcPort dst dstPort
       case mbTcb of
         -- XXX need to wait until the connection is finalized
         Just tcpTcb -> return TcpSocket { .. }
         Nothing     -> throwSE dst (AlreadyConnected src srcPort dst dstPort)

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




-- Utilities -------------------------------------------------------------------

-- | Raise an exception when no route can be found to the destination.
route :: Network addr
      => NetworkStack -> Maybe Device -> addr -> addr -> IO (RouteInfo addr)

route ns mbDev src dst =
  do mbRoute <- route' ns mbDev src dst
     case mbRoute of
       Just ri -> return ri
       Nothing -> throwSE dst NoRouteToHost


-- | Return source routing information, when a route exists to the destination.
route' :: Network addr
       => NetworkStack -> Maybe Device -> addr -> addr
       -> IO (Maybe (RouteInfo addr))

route' ns mbDev src dst =
  do mbRoute <- lookupRoute ns dst
     case mbRoute of
       Just ri | maybe True (riDev ri ==) mbDev
                 && (src == riSource ri || isWildcardAddr src) ->
                 return (Just ri)

       _ -> return Nothing
