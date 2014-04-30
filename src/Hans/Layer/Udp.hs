{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Layer.Udp (
    UdpHandle
  , UdpException
  , runUdpLayer

  , queueUdp
  , sendUdp
  , Handler
  , addUdpHandler
  , addUdpHandlerAnyPort
  , removeUdpHandler
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Message.Icmp4
import Hans.Message.Ip4
import Hans.Message.Udp
import Hans.Ports
import Hans.Utils
import qualified Hans.Layer.IP4 as IP4
import qualified Hans.Layer.Icmp4 as Icmp4

import Control.Concurrent (forkIO,newEmptyMVar,takeMVar,putMVar)
import Control.Monad (guard,mplus,when)
import Data.Maybe (isNothing)
import Data.Serialize.Get (runGet)
import Data.Typeable (Typeable)
import MonadLib (get,set)
import qualified Control.Exception    as X
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


type Handler = IP4 -> UdpPort -> S.ByteString -> IO ()

type UdpHandle = Channel (Udp ())

data UdpException = NoPortsAvailable
                  | PortInUse UdpPort
                    deriving (Show,Typeable)

instance X.Exception UdpException

runUdpLayer :: UdpHandle -> IP4.IP4Handle -> Icmp4.Icmp4Handle -> IO ()
runUdpLayer h ip4 icmp4 = do
  IP4.addIP4Handler ip4 udpProtocol (queueUdp h)
  void (forkIO (loopLayer "udp" (emptyUdp4State ip4 icmp4) (receive h) id))

-- | Send a UDP datagram.  When the source port is given, this will send using
-- that source port.  If the source port is not given, a fresh one is used, and
-- immediately recycled.
--
-- NOTE: this doesn't prevent you from sending messages on a port that another
-- thread is already using.  This is a funky design, and we'd be better suited
-- by introducing a UdpSocket type.
sendUdp :: UdpHandle -> IP4 -> Maybe UdpPort -> UdpPort -> L.ByteString -> IO ()
sendUdp h !dst (Just sp) !dp !bs =
  send h (handleOutgoing dst sp dp bs)
sendUdp h !dst Nothing !dp !bs = do
  res <- newEmptyMVar

  send h $ do e <- allocPort
              case e of
                Right sp ->
                  do handleOutgoing dst sp dp bs
                     freePort sp
                     output (putMVar res Nothing)

                Left err -> output (putMVar res (Just err))

  mbErr <- takeMVar res
  case mbErr of
    Nothing  -> return ()
    Just err -> X.throwIO err

-- | Queue an incoming udp message from the IP4 layer.
queueUdp :: UdpHandle -> IP4Header -> S.ByteString -> IO ()
queueUdp h !ip4 !bs = send h (handleIncoming ip4 bs)

-- | Add a handler for incoming udp datagrams on a specific port.
addUdpHandler :: UdpHandle -> UdpPort -> Handler -> IO ()
addUdpHandler h sp k = do
  res <- newEmptyMVar
  send h $ do mb <- reservePort sp
              when (isNothing mb) (addHandler sp k)
              output (putMVar res mb)
  mb <- takeMVar res
  case mb of
    Nothing  -> return ()
    Just err -> X.throwIO err

-- | Add a handler for incoming udp datagrams on a freshly allocated port.
addUdpHandlerAnyPort :: UdpHandle -> (UdpPort -> Handler) -> IO UdpPort
addUdpHandlerAnyPort h k = do
  res <- newEmptyMVar
  send h $ do e <- allocPort
              case e of
                Right sp -> addHandler sp (k sp)
                Left _   -> return ()
              output (putMVar res e)

  e <- takeMVar res
  case e of
    Right sp -> return sp
    Left err -> X.throwIO err

-- | Remove a handler present on the port given.
removeUdpHandler :: UdpHandle -> UdpPort -> IO ()
removeUdpHandler h !sp = send h $ do freePort sp
                                     removeHandler sp


-- Udp State -------------------------------------------------------------------

type Udp = Layer UdpState

data UdpState = UdpState
  { udpPorts       :: PortManager UdpPort
  , udpHandlers    :: Handlers UdpPort Handler
  , udpIp4Handle   :: IP4.IP4Handle
  , udpIcmp4Handle :: Icmp4.Icmp4Handle
  }

emptyUdp4State :: IP4.IP4Handle -> Icmp4.Icmp4Handle -> UdpState
emptyUdp4State ip4 icmp4 = UdpState
  { udpPorts       = emptyPortManager [maxBound, maxBound - 1 .. 1 ]
  , udpHandlers    = emptyHandlers
  , udpIp4Handle   = ip4
  , udpIcmp4Handle = icmp4
  }

instance ProvidesHandlers UdpState UdpPort Handler where
  getHandlers      = udpHandlers
  setHandlers hs s = s { udpHandlers = hs }


-- Utilities -------------------------------------------------------------------

modifyPortManager :: (PortManager UdpPort -> (a,PortManager UdpPort)) -> Udp a
modifyPortManager f = do
  state <- get
  let (a,pm') = f (udpPorts state)
  pm' `seq` set state { udpPorts = pm' }
  return a

ip4Handle :: Udp IP4.IP4Handle
ip4Handle  = udpIp4Handle `fmap` get

icmp4Handle :: Udp Icmp4.Icmp4Handle
icmp4Handle  = udpIcmp4Handle `fmap` get

allocPort :: Udp (Either UdpException UdpPort)
allocPort = modifyPortManager $ \pm ->
              case nextPort pm of
                Just (p,pm') -> (Right p,pm')
                Nothing      -> (Left NoPortsAvailable,pm)

reservePort :: UdpPort -> Udp (Maybe UdpException)
reservePort sp = modifyPortManager $ \ pm ->
                   case reserve sp pm of
                     Just pm' -> (Nothing,pm')
                     Nothing  -> (Just (PortInUse sp), pm)

freePort :: UdpPort -> Udp ()
freePort sp = modifyPortManager $ \ pm ->
                case unreserve sp pm of
                  Just pm' -> ((), pm')
                  Nothing  -> ((), pm )


-- Message Handling ------------------------------------------------------------

handleIncoming :: IP4Header -> S.ByteString -> Udp ()
handleIncoming ip4 bs = do
  let src = ip4SourceAddr ip4
  guard (validateUdpChecksum src (ip4DestAddr ip4) bs)
  (hdr,bytes) <- liftRight (runGet parseUdpPacket bs)
  listening src hdr bytes `mplus` unreachable ip4 bs

listening :: IP4 -> UdpHeader -> S.ByteString -> Udp ()
listening src hdr bytes = do
  h <- getHandler (udpDestPort hdr)
  output $ do _ <- forkIO (h src (udpSourcePort hdr) bytes)
              return ()

-- | Deliver a destination unreachable mesasge, via the icmp layer.
unreachable :: IP4Header -> S.ByteString -> Udp ()
unreachable hdr orig = do
  icmp4 <- icmp4Handle
  output (Icmp4.destUnreachable icmp4 PortUnreachable hdr (S.length orig) orig)

handleOutgoing :: IP4 -> UdpPort -> UdpPort -> L.ByteString -> Udp ()
handleOutgoing dst sp dp bs = do
  ip4 <- ip4Handle
  let hdr = UdpHeader sp dp 0
  output $ IP4.withIP4Source ip4 dst $ \ src -> do
    let ip4Hdr = emptyIP4Header
          { ip4DestAddr     = dst
          , ip4Protocol     = udpProtocol
          , ip4DontFragment = False
          }
    pkt <- renderUdpPacket hdr bs (mkIP4PseudoHeader src dst udpProtocol)
    IP4.sendIP4Packet ip4 ip4Hdr pkt
