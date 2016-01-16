{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hans.Socket.Udp where

import           Hans.Addr (toAddr,fromAddr,isBroadcastAddr)
import qualified Hans.Buffer.Datagram as DGram
import           Hans.Device.Types (Device)
import           Hans.Lens (view,to)
import           Hans.Network (Network,RouteInfo(..))
import           Hans.Socket.Types
import           Hans.Types
import           Hans.Udp.Output (primSendUdp)

import           Control.Exception (throwIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,readIORef)


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
                                 Nothing   -> throwIO NoPortAvailable

                 Just p  -> return p

     udpSockState <- newIORef (KnownSource mbDev src srcPort)

     udpBuffer <- DGram.newBuffer scRecvBufferSize

     -- XXX: Need some SocketError exceptions: this only happens if there's
     -- already something listening
     mbClose  <- registerRecv udpNS (toAddr src) srcPort udpBuffer
     udpClose <- case mbClose of
                   Just unreg -> return unreg
                   Nothing    -> throwIO AlreadyListening

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
                                    Nothing   -> throwIO NoPortAvailable

       udpSockState <- newIORef (KnownRoute ri dst srcPort dstPort)

       udpBuffer <- DGram.newBuffer scRecvBufferSize

       mbClose  <- registerRecv udpNS (toAddr src) srcPort udpBuffer
       udpClose <- case mbClose of
                     Just unreg -> return unreg
                     Nothing    -> throwIO AlreadyConnected

       return UdpSocket { .. }

  sWrite UdpSocket { .. } bytes =
    do path <- readIORef udpSockState
       case path of

         KnownRoute ri dst srcPort dstPort ->
           do sent <- primSendUdp udpNS ri dst srcPort dstPort bytes
              if sent
                 then return (fromIntegral (L.length bytes))
                 else return (-1)

         KnownSource{} ->
              throwIO NoConnection

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
                  throwIO NoRouteToHost

       -- we can't use sendto if sConnect has been used already
       KnownRoute{} ->
         throwIO AlreadyConnected
{-# INLINE sendto #-}


