{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Hans.Layer.Udp (
    UdpHandle
  , runUdpLayer

  , queueUdp
  , sendUdp
  , Handler
  , addUdpHandler
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

import Control.Concurrent (forkIO)
import Control.Monad (guard,mplus)
import Data.Serialize.Get (runGet)
import MonadLib (get,set)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


type Handler = IP4 -> UdpPort -> S.ByteString -> IO ()

type UdpHandle = Channel (Udp ())

runUdpLayer :: UdpHandle -> IP4.IP4Handle -> Icmp4.Icmp4Handle -> IO ()
runUdpLayer h ip4 icmp4 = do
  IP4.addIP4Handler ip4 udpProtocol (queueUdp h)
  void (forkIO (loopLayer "udp" (emptyUdp4State ip4 icmp4) (receive h) id))

sendUdp :: UdpHandle -> IP4 -> Maybe UdpPort -> UdpPort -> L.ByteString -> IO ()
sendUdp h !dst mb !dp !bs = send h (handleOutgoing dst mb dp bs)

queueUdp :: UdpHandle -> IP4Header -> S.ByteString -> IO ()
queueUdp h !ip4 !bs = send h (handleIncoming ip4 bs)

addUdpHandler :: UdpHandle -> UdpPort -> Handler -> IO ()
addUdpHandler h !sp k = send h (handleAddHandler sp k)

removeUdpHandler :: UdpHandle -> UdpPort -> IO ()
removeUdpHandler h !sp = send h (handleRemoveHandler sp)


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

ip4Handle :: Udp IP4.IP4Handle
ip4Handle  = udpIp4Handle `fmap` get

icmp4Handle :: Udp Icmp4.Icmp4Handle
icmp4Handle  = udpIcmp4Handle `fmap` get

maybePort :: Maybe UdpPort -> Udp UdpPort
maybePort (Just p) = return p
maybePort Nothing  = do
  state   <- get
  (p,pm') <- nextPort (udpPorts state)
  pm' `seq` set state { udpPorts = pm' }
  return p

-- Message Handling ------------------------------------------------------------

handleAddHandler :: UdpPort -> Handler -> Udp ()
handleAddHandler sp k = do
  state <- get
  pm'   <- reserve sp (udpPorts state)
  pm' `seq` set state { udpPorts = pm' }
  addHandler sp k

handleRemoveHandler :: UdpPort -> Udp ()
handleRemoveHandler sp = do
  state <- get
  pm'   <- unreserve sp (udpPorts state)
  pm' `seq` set state { udpPorts = pm' }
  removeHandler sp

handleIncoming :: IP4Header -> S.ByteString -> Udp ()
handleIncoming ip4 bs = do
  let src = ip4SourceAddr ip4
  guard (validateUdpChecksum src (ip4DestAddr ip4) bs)
  (hdr,bytes) <- liftRight (runGet parseUdpPacket bs)
  listening src hdr bytes `mplus` unreachable ip4 bs

listening :: IP4 -> UdpHeader -> S.ByteString -> Udp ()
listening src hdr bytes = do
  h <- getHandler (udpDestPort hdr)
  output (h src (udpSourcePort hdr) bytes)

-- | Deliver a destination unreachable mesasge, via the icmp layer.
unreachable :: IP4Header -> S.ByteString -> Udp ()
unreachable hdr orig = do
  icmp4 <- icmp4Handle
  output (Icmp4.destUnreachable icmp4 PortUnreachable hdr orig)

handleOutgoing :: IP4 -> Maybe UdpPort -> UdpPort -> L.ByteString -> Udp ()
handleOutgoing dst mb dp bs = do
  sp  <- maybePort mb
  ip4 <- ip4Handle
  let hdr = UdpHeader sp dp 0
  output $ IP4.withIP4Source ip4 dst $ \ src -> do
    pkt <- renderUdpPacket hdr bs (mkIP4PseudoHeader src dst udpProtocol)
    IP4.sendIP4Packet ip4 udpProtocol dst pkt
