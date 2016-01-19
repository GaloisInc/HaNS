{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Socket.Types where

import Hans.Addr (isWildcardAddr)
import Hans.Device.Types (Device)
import Hans.Network (Network(..),RouteInfo(..))
import Hans.Types (HasNetworkStack,NetworkStack)

import           Control.Exception (Exception,throwIO)
import qualified Data.ByteString.Lazy as L
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

data ConnectionException = AlreadyConnected
                           -- ^ This connection already exists.

                         | NoConnection
                           -- ^ No information about the other end of the
                           -- socket was present.

                         | NoPortAvailable
                           -- ^ All ports are in use.

                         | ConnectionRefused

                         | ConnectionClosing

                         | DoesNotExist
                           -- ^ The connection is already closed.
                           deriving (Show,Typeable)

data ListenException = AlreadyListening
                       -- ^ Something is already listening on this
                       -- host/port combination.
                       deriving (Show,Typeable)

data RoutingException = NoRouteToHost
                        -- ^ It's not possible to reach this host from this
                        -- source address.
                        deriving (Show,Typeable)

instance Exception ConnectionException
instance Exception ListenException
instance Exception RoutingException


-- Utilities -------------------------------------------------------------------

-- | Raise an exception when no route can be found to the destination.
route :: Network addr
      => NetworkStack -> Maybe Device -> addr -> addr -> IO (RouteInfo addr)

route ns mbDev src dst =
  do mbRoute <- route' ns mbDev src dst
     case mbRoute of
       Just ri -> return ri
       Nothing -> throwIO NoRouteToHost


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
