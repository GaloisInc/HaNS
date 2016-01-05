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
import           Hans.Lens
import           Hans.Network
import           Hans.Types
                     (HasNetworkStack(..),NetworkStack,registerRecv,UdpBuffer
                     ,nextUdpPort)
import           Hans.Udp.Input ()
import           Hans.Udp.Output (primSendUdp)

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
           -> Maybe addr     -- ^ Local address
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

data SocketException addr = AlreadyOpen !addr !SockPort
                            -- ^ Something is already listening on this
                            -- host/port combination.

                          | AlreadyConnected
                            -- ^ The socket is already connected to another
                            -- host.

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
                   Nothing    -> throwIO (AlreadyOpen src srcPort)

     return $! UdpSocket { .. }


instance DataSocket UdpSocket where

  -- Always lookup the route before modifying the state of the socket, so that
  -- the state can be manipulated atomically.
  sConnect ns SocketConfig { .. } mbDev mbSrc mbPort dst dstPort =
    do let udpNS = view networkStack ns

       mbRoute <- lookupRoute udpNS dst
       ri      <- case mbRoute of
                    Just ri | maybe True (riDev ri ==) mbDev -> return ri
                    _                                        -> throwSE dst NoRouteToHost

       src <- case mbSrc of
                Just src | src == riSource ri -> return src
                _                             -> throwSE dst NoRouteToHost

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
                     Nothing    -> throwIO (AlreadyOpen src srcPort)

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
         do mbRoute <- lookupRoute udpNS dst
            let sameDev = case mbDev of
                            Just dev' -> \ ri -> dev' == riDev ri
                            Nothing   -> \ _  -> True

            case mbRoute of
              -- the device holds the same address we were expecting to send
              -- from, or we're sending from the wildcard address
              Just ri
                | sameDev ri && (src == riSource ri || isWildcardAddr src) ->
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
       KnownRoute {} ->
         throwSE dst AlreadyConnected
{-# INLINE sendto #-}


-- TCP Sockets -----------------------------------------------------------------

data TcpSocket addr = TcpSocket { tcpNS :: !NetworkStack
                                }

instance Socket TcpSocket where

  sClose sock = undefined


instance DataSocket TcpSocket where

  sConnect ns mbDev src mbSrcPort dst dstPort = undefined

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
