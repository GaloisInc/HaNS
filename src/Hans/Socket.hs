{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Socket where

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

class Socket sock where

  -- | Open a socket. This is the functionality of `socket` and `bind` in the
  -- same operation.
  sOpen :: (HasNetworkStack ns, Network addr)
        => ns
        -> SocketConfig
        -> Maybe Device -> addr -> Maybe SockPort -> IO (sock addr)

  -- | Connect this socket to one on a remote machine.
  sConnect :: Network addr => sock addr -> addr -> SockPort -> IO ()

  -- | Send a chunk of data on a socket.
  sWrite :: Network addr => sock addr -> L.ByteString -> IO Int

  -- | Read a chunk of data from a socket. Reading an empty result indicates
  -- that the socket has closed.
  sRead :: Network addr => sock addr -> Int -> IO L.ByteString

  -- | Non-blocking read from a socket. Reading an empty result means that the
  -- socket has closed, while reading a 'Nothing' result indicates that there
  -- was no data available.
  sTryRead :: Network addr => sock addr -> Int -> IO (Maybe L.ByteString)

  -- | Close an open socket.
  sClose :: Network addr => sock addr -> IO ()


data SockState addr = KnownRoute !(RouteInfo addr) !addr !SockPort !SockPort
                      -- ^ Cached route, and port information

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

instance (Show addr, Typeable addr) => Exception (SocketException addr)

throwSE :: (Show addr, Typeable addr) => addr -> SocketException addr -> IO a
throwSE _ = throwIO
{-# INLINE throwSE #-}


-- UDP Sockets -----------------------------------------------------------------

data UdpSocket addr = UdpSocket { udpNS        :: !NetworkStack
                                , udpBuffer    :: !UdpBuffer
                                , udpSockState :: !(IORef (SockState addr))
                                , udpClose     :: !(IO ())
                                }

instance HasNetworkStack (UdpSocket addr) where
  networkStack = to udpNS
  {-# INLINE networkStack #-}

instance Socket UdpSocket where

  sOpen ns SocketConfig { .. } mbDev src mbPort =
    do let udpNS = view networkStack ns
       srcPort <- case mbPort of
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

  -- Always lookup the route before modifying the state of the socket, so that
  -- the state can be manipulated atomically.
  sConnect UdpSocket { .. } dst dstPort =
    do mbRoute <- lookupRoute udpNS dst
       case mbRoute of

         Just ri ->

           do let sameDev (Just dev') = riDev ri == dev'
                  sameDev Nothing     = True

                  connect state = case state of
                    KnownSource mbDev src' srcPort

                        -- If the source was the wildcard address, change it to
                        -- the address specified by that rule.
                      | sameDev mbDev && (riSource ri == src' || isWildcardAddr src') ->
                        (KnownRoute ri dst srcPort dstPort, Nothing)

                      | otherwise ->
                        (state, Just NoRouteToHost)

                    _ ->
                      (state, Just AlreadyConnected)

              mbEx <- atomicModifyIORef' udpSockState connect
              case mbEx of
                Nothing -> return ()
                Just e  -> throwSE dst e

         Nothing -> throwSE dst NoRouteToHost

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


  sClose UdpSocket { .. } = udpClose
  {-# INLINE sClose #-}


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
