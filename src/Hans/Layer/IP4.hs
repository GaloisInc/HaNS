{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Hans.Layer.IP4 (
    IP4Handle
  , runIP4Layer
  , Rule(..)

    -- * External Interface
  , connectEthernet
  , withIP4Source
  , sendIP4Packet
  , addIP4RoutingRule
  , addIP4Handler
  , removeIP4Handler
  ) where

import Hans.Address
import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Arp
import Hans.Layer.Ethernet
import Hans.Layer.IP4.Fragmentation
import Hans.Layer.IP4.Routing
import Hans.Message.EthernetFrame
import Hans.Message.Ip4
import Hans.Utils
import Hans.Utils.Checksum

import Control.Concurrent (forkIO)
import Control.Monad (guard,mplus,(<=<))
import Data.Serialize.Get (runGet)
import MonadLib (get,set)
import qualified Data.ByteString as S


type Handler = IP4 -> IP4 -> Packet -> IO ()

type IP4Handle = Channel (IP ())

runIP4Layer :: IP4Handle -> ArpHandle -> EthernetHandle -> IO ()
runIP4Layer h arp eth = do
  void (forkIO (loopLayer (emptyIP4State arp) (receive h) id))
  connectEthernet h eth

connectEthernet :: IP4Handle -> EthernetHandle -> IO ()
connectEthernet h eth =
  addEthernetHandler eth (EtherType 0x0800) (send h . handleIncoming)

withIP4Source :: IP4Handle -> IP4 -> (IP4 -> IO ()) -> IO ()
withIP4Source h !dst k = send h (handleSource dst k)

addIP4RoutingRule :: IP4Handle -> Rule IP4Mask IP4 -> IO ()
addIP4RoutingRule h !rule = send h (handleAddRule rule)

sendIP4Packet :: IP4Handle -> IP4Protocol -> IP4 -> Packet -> IO ()
sendIP4Packet h !prot !dst !pkt = send h (handleOutgoing prot dst pkt)

addIP4Handler :: IP4Handle -> IP4Protocol -> Handler -> IO ()
addIP4Handler h !prot k = send h (addHandler prot k)

removeIP4Handler :: IP4Handle -> IP4Protocol -> IO ()
removeIP4Handler h !prot = send h (removeHandler prot)

-- IP4 State -------------------------------------------------------------------

type IP = Layer IP4State

data IP4State = IP4State
  { ip4Fragments :: FragmentationTable IP4
  , ip4Routes    :: RoutingTable IP4
  , ip4Handlers  :: Handlers IP4Protocol Handler
  , ip4NextIdent :: Ident
  , ip4ArpHandle :: ArpHandle
  }

instance ProvidesHandlers IP4State IP4Protocol Handler where
  getHandlers      = ip4Handlers
  setHandlers hs i = i { ip4Handlers = hs }


emptyIP4State :: ArpHandle -> IP4State
emptyIP4State arp = IP4State
  { ip4Fragments = emptyFragmentationTable
  , ip4Routes    = emptyRoutingTable
  , ip4Handlers  = emptyHandlers
  , ip4NextIdent = 0
  , ip4ArpHandle = arp
  }


-- IP4 Utilities ---------------------------------------------------------------

arpHandle :: IP ArpHandle
arpHandle  = ip4ArpHandle `fmap` get

sendBytes :: IP4Protocol -> IP4 -> Packet -> IP ()
sendBytes prot dst bs = do
  rule@(src,_,mtu) <- findRoute dst
  let hdr = emptyIP4Header prot src dst
  hdr' <- if fromIntegral (S.length bs) + 20 < mtu
    then return hdr
    else do
      i <- nextIdent
      return (setIdent i hdr)
  sendPacket' (IP4Packet hdr' bs) rule

sendPacket :: IP4Packet -> IP ()
sendPacket pkt = do
  rule@(src,_,_) <- findRoute (ip4DestAddr (ip4Header pkt))
  guard (src /= ip4SourceAddr (ip4Header pkt))
  sendPacket' pkt rule

-- | Send a packet using a given routing rule
sendPacket' :: IP4Packet -> (IP4,IP4,Mtu) -> IP ()
sendPacket' pkt (src,dst,mtu) = do
  arp  <- arpHandle
  output $ do
    let frags = splitPacket mtu pkt
    mapM_ (arpIP4Packet arp src dst <=< renderIP4Packet) frags


-- | Find a route to an address
findRoute :: IP4 -> IP (IP4,IP4,Mtu)
findRoute addr = do
  state <- get
  just (route addr (ip4Routes state))


-- | Route a packet that is forwardable
forward :: IP4Packet -> IP ()
forward pkt = sendPacket pkt


-- | Require that an address is local.
localAddress :: IP4 -> IP ()
localAddress ip = do
  state <- get
  guard (ip `elem` localAddrs (ip4Routes state))


findSourceMask :: IP4 -> IP IP4Mask
findSourceMask ip = do
  state <- get
  just (sourceMask ip (ip4Routes state))


broadcastDestination :: IP4 -> IP ()
broadcastDestination ip = do
  mask <- findSourceMask ip
  guard (isBroadcast mask ip)

-- | Route a message to a local handler
routeLocal :: IP4Packet -> IP ()
routeLocal pkt@(IP4Packet hdr _) = do
  let dest = ip4DestAddr hdr
  localAddress dest `mplus` broadcastDestination dest
  h  <- getHandler (ip4Protocol hdr)
  mb <- handleFragments pkt
  case mb of
    Nothing -> return ()
    Just bs -> output (h (ip4SourceAddr hdr) (ip4DestAddr hdr) bs)


handleFragments :: IP4Packet -> IP (Maybe Packet)
handleFragments pkt = do
  state <- get
  now   <- time
  let (table',mb) = processIP4Packet now (ip4Fragments state) pkt
  table' `seq` set state { ip4Fragments = table' }
  return mb

nextIdent :: IP Ident
nextIdent = do
  state <- get
  let i = ip4NextIdent state
  set state { ip4NextIdent = i + 1 }
  return i

-- Message Handling ------------------------------------------------------------

-- | Incoming packet from the network
handleIncoming :: Packet -> IP ()
handleIncoming bs = do
  (hdr,hlen,plen) <- liftRight (runGet parseIP4Packet bs)
  let (header,rest) = S.splitAt hlen bs
  let payload       = S.take plen rest
  let checksum      = computeChecksum 0 header
  let pkt           = IP4Packet hdr payload
  guard $ and
    [ S.length bs       >= 20
    , hlen              >= 20
    , checksum          == 0
    , ip4Version hdr    == 4
    ]

  -- forward?
  routeLocal pkt `mplus` forward pkt


-- | Outgoing packet
handleOutgoing :: IP4Protocol -> IP4 -> Packet -> IP ()
handleOutgoing prot dst bs = do
  sendBytes prot dst bs


handleAddRule :: Rule IP4Mask IP4 -> IP ()
handleAddRule rule = do
  state <- get
  let routes' = addRule rule (ip4Routes state)
  routes' `seq` set state { ip4Routes = routes' }


handleSource :: IP4 -> (IP4 -> IO ()) -> IP ()
handleSource dst k = do
  (s,_,_) <- findRoute dst
  output (k s)
