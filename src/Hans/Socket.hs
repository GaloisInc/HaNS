{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Socket where

import qualified Hans.Buffer.Datagram as DGram
import           Hans.Device.Types (Device(devName))
import           Hans.IP4.Packet (IP4,pattern WildcardIP4,pattern BroadcastIP4)
import           Hans.Types
                     (HasNetworkStack(..),NetworkStack,registerRecv4,UdpBuffer
                     ,lookupRoute,nextUdpPort4)
import           Hans.Udp.Input ()
import           Hans.Udp.Output (primSendUdp4)

import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,readIORef,atomicModifyIORef')
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

class Socket (sock :: * -> *) addr where

  -- | Open a socket. This is the functionality of `socket` and `bind` in the
  -- same operation.
  sOpen :: HasNetworkStack ns
        => ns
        -> SocketConfig
        -> Maybe Device -> addr -> Maybe SockPort -> IO (sock addr)

  -- | Connect this socket to one on a remote machine.
  sConnect :: sock addr -> addr -> SockPort -> IO ()

  -- | Send a chunk of data on a socket.
  sWrite :: sock addr -> L.ByteString -> IO Int

  -- | Read a chunk of data from a socket. Reading an empty result indicates
  -- that the socket has closed.
  sRead :: sock addr -> Int -> IO L.ByteString

  -- | Non-blocking read from a socket. Reading an empty result means that the
  -- socket has closed, while reading a 'Nothing' result indicates that there
  -- was no data available.
  sTryRead :: sock addr -> Int -> IO (Maybe L.ByteString)

  -- | Close an open socket.
  sClose :: sock addr -> IO ()


data SockState addr = KnownRoute !Device !addr !SockPort !addr !SockPort !addr
                      -- ^ Known source, dest and next-hop.

                    | KnownSource !(Maybe Device) !addr !SockPort
                      -- ^ Known source only.


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

throwIO4 :: SocketException IP4 -> IO a
throwIO4  = X.throwIO

instance (Show addr, Typeable addr) => X.Exception (SocketException addr)


-- Datagram Sockets ------------------------------------------------------------

data DatagramSocket addr =
  DatagramSocket { dgNS       :: !NetworkStack
                 , dgBuffer   :: !(UdpBuffer addr)
                 , dgState    :: !(IORef (SockState addr))
                 , dgClose    ::  IO ()
                 }

instance HasNetworkStack (DatagramSocket addr) where
  getNetworkStack DatagramSocket { .. } = dgNS
  {-# INLINE getNetworkStack #-}

instance Socket DatagramSocket IP4 where

  sOpen ns SocketConfig { .. } mbDev src port =
    do let dgNS = getNetworkStack ns
       srcPort <- case port of
                   Nothing -> do mb <- nextUdpPort4 dgNS src
                                 case mb of
                                   Just port -> return port
                                   Nothing   -> throwIO4 NoPortAvailable

                   Just p  -> return p

       dgState  <- newIORef (KnownSource mbDev src srcPort)

       dgBuffer <- DGram.newBuffer scRecvBufferSize

       -- XXX: Need some SocketError exceptions: this only happens if there's
       -- already something listening
       mbClose <- registerRecv4 dgNS src srcPort dgBuffer
       dgClose <- case mbClose of
                    Just unreg -> return unreg
                    Nothing    -> throwIO4 (AlreadyOpen src srcPort)

       return $! DatagramSocket { .. }

  sConnect sock @ DatagramSocket { .. } dst dstPort =
    do mbRoute <- lookupRoute dgNS dst
       case mbRoute of

         Just (src,next,dev) ->

           do let sameDev (Just dev') = devName dev == devName dev'
                  sameDev Nothing     = True

                  connect state = case state of
                    KnownSource mbDev src' srcPort

                        -- If the source was the wildcard address, change it to
                        -- the address specified by that rule.
                      | (src == src' && sameDev mbDev) || src' == WildcardIP4 ->
                        (KnownRoute dev src srcPort dst dstPort next, Nothing)

                      | otherwise ->
                        (state, Just NoRouteToHost)

                    _ ->
                      (state, Just AlreadyConnected)

              mbEx <- atomicModifyIORef' dgState connect
              case mbEx of
                Nothing -> return ()
                Just e  -> throwIO4 e

         Nothing -> throwIO4 NoRouteToHost

  sWrite sock @ DatagramSocket { .. } bytes =
    do path <- readIORef dgState
       case path of

         KnownRoute dev src srcPort dst dstPort next ->
           do sent <- primSendUdp4 dgNS dev src srcPort dst dstPort next bytes
              if sent
                 then return (fromIntegral (L.length bytes))
                 else return (-1)

         KnownSource{} ->
              throwIO4 NoConnection

  sRead DatagramSocket { .. } len =
    do (_,buf) <- DGram.readChunk dgBuffer
       return (L.fromStrict (S.take len buf))

  sTryRead DatagramSocket { .. } len =
    do mb <- DGram.tryReadChunk dgBuffer
       case mb of
         Just (_,buf) -> return $! Just $! L.fromStrict $! S.take len buf
         Nothing      -> return Nothing


  sClose DatagramSocket { .. } = dgClose
  {-# INLINE sClose #-}


-- | Receive, with information about who sent this datagram.
recvfrom4 :: DatagramSocket IP4 -> IO (Device,IP4,SockPort,L.ByteString)
recvfrom4 DatagramSocket { .. } =
  do ((dev,src,srcPort,_,_), chunk) <- DGram.readChunk dgBuffer
     return (dev,src,srcPort,L.fromStrict chunk)
{-# LANGUAGE recvfrom4 #-}


-- | Send to a specific end host.
sendto4 :: DatagramSocket IP4 -> IP4 -> SockPort -> L.ByteString -> IO ()
sendto4 sock @ DatagramSocket { .. } = \ dst dstPort bytes ->
  do state <- readIORef dgState
     case state of

       KnownSource mbDev src srcPort ->
         do mbRoute <- lookupRoute dgNS dst
            let sameDev = case mbDev of
                            Just dev' -> \ dev -> devName dev == devName dev'
                            Nothing   -> \ _   -> True

            case mbRoute of
              Just (src',next,dev)
                | (src == src' && sameDev dev) || src == WildcardIP4 ->
                  do _ <- primSendUdp4 dgNS dev src' srcPort dst dstPort next bytes
                     return ()

              Nothing
                | Just dev <- mbDev, dst == BroadcastIP4 ->
                  do _ <- primSendUdp4 dgNS dev src srcPort dst dstPort dst bytes
                     return ()

              _ ->
                  throwIO4 NoRouteToHost

       -- we can't use sendto if sConnect has been used already
       KnownRoute {} ->
         throwIO4 AlreadyConnected
{-# INLINE sendto4 #-}

