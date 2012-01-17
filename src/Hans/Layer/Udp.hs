{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Hans.Layer.Udp (
    UdpHandle
  , runUdpLayer

  , queueUdp
  , sendUdp
  , addUdpHandler
  , removeUdpHandler
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.IP4
import Hans.Layer.Icmp4
import Hans.Message.Ip4
import Hans.Message.Udp
import Hans.Ports
import Hans.Utils

import Control.Concurrent (forkIO)
import Data.Serialize.Get (runGet)
import MonadLib (get,set)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


type Handler = IP4 -> UdpPort -> S.ByteString -> IO ()

type UdpHandle = Channel (Udp ())

udpProtocol :: IP4Protocol
udpProtocol  = IP4Protocol 0x11

runUdpLayer :: UdpHandle -> IP4Handle -> Icmp4Handle -> IO ()
runUdpLayer h ip4 icmp4 = do
  addIP4Handler ip4 udpProtocol (queueUdp h)
  void (forkIO (loopLayer (emptyUdp4State ip4 icmp4) (receive h) id))

sendUdp :: UdpHandle -> IP4 -> Maybe UdpPort -> UdpPort -> L.ByteString -> IO ()
sendUdp h !dst mb !dp !bs = send h (handleOutgoing dst mb dp bs)

queueUdp :: UdpHandle -> IP4 -> IP4 -> S.ByteString -> IO ()
queueUdp h !src !dst !bs = send h (handleIncoming src dst bs)

addUdpHandler :: UdpHandle -> UdpPort -> Handler -> IO ()
addUdpHandler h !sp k = send h (handleAddHandler sp k)

removeUdpHandler :: UdpHandle -> UdpPort -> IO ()
removeUdpHandler h !sp = send h (handleRemoveHandler sp)


-- Udp State -------------------------------------------------------------------

type Udp = Layer UdpState

data UdpState = UdpState
  { udpPorts       :: PortManager UdpPort
  , udpHandlers    :: Handlers UdpPort Handler
  , udpIp4Handle   :: IP4Handle
  , udpIcmp4Handle :: Icmp4Handle
  }

emptyUdp4State :: IP4Handle -> Icmp4Handle -> UdpState
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

ip4Handle :: Udp IP4Handle
ip4Handle  = udpIp4Handle `fmap` get

--icmp4Handle :: Udp Icmp4Handle
--icmp4Handle  = udpIcmp4Handle `fmap` get

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


handleIncoming :: IP4 -> IP4 -> S.ByteString -> Udp ()
handleIncoming src _dst bs = do
  (hdr,pkt) <- liftRight (runGet parseUdpPacket bs)
  h         <- getHandler (udpDestPort hdr)
  output (h src (udpSourcePort hdr) pkt)


handleOutgoing :: IP4 -> Maybe UdpPort -> UdpPort -> L.ByteString -> Udp ()
handleOutgoing dst mb dp bs = do
  sp  <- maybePort mb
  ip4 <- ip4Handle
  let hdr = UdpHeader sp dp 0
  output $ withIP4Source ip4 dst $ \ src -> do
    pkt <- renderUdpPacket hdr bs (mkIP4PseudoHeader src dst udpProtocol)
    sendIP4Packet ip4 udpProtocol dst pkt
