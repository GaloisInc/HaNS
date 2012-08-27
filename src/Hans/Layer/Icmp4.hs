{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Hans.Layer.Icmp4 (
    Icmp4Handle
  , runIcmp4Layer
  , addIcmp4Handler
  , destUnreachable
  ) where

import Hans.Address.IP4 (IP4)
import Hans.Channel
import Hans.Layer
import Hans.Message.Icmp4
import Hans.Message.Ip4
import Hans.Utils
import qualified Hans.Layer.IP4 as IP4

import Control.Concurrent (forkIO)
import Data.Serialize (runGet,runPutLazy,runPut,putByteString)
import MonadLib (get,set)
import qualified Data.ByteString as S

type Handler = Icmp4Packet -> IO ()

type Icmp4Handle = Channel (Icmp4 ())

icmpProtocol :: IP4Protocol
icmpProtocol  = IP4Protocol 0x1

runIcmp4Layer :: Icmp4Handle -> IP4.IP4Handle -> IO ()
runIcmp4Layer h ip4 = do
  let handles = Icmp4Handles ip4 []
  IP4.addIP4Handler ip4 icmpProtocol
    $ \ hdr bs -> send h (handleIncoming hdr bs)
  void (forkIO (loopLayer "icmp4" handles (receive h) id))

data Icmp4Handles = Icmp4Handles
  { icmpIp4      :: IP4.IP4Handle
  , icmpHandlers :: [Handler]
  }

type Icmp4 = Layer Icmp4Handles

ip4Handle :: Icmp4 IP4.IP4Handle
ip4Handle  = icmpIp4 `fmap` get

-- | Add a handler for Icmp4 messages that match the provided predicate.
addIcmp4Handler :: Icmp4Handle -> Handler -> IO ()
addIcmp4Handler h k = send h (handleAdd k)

-- | Send a destination unreachable message to a host, with the given bytes as
-- its body.
destUnreachable :: Icmp4Handle -> DestinationUnreachableCode
                -> IP4Header -> S.ByteString -> IO ()
destUnreachable h code hdr body =
  send h $ sendPacket True (ip4SourceAddr hdr)
         $ DestinationUnreachable code bytes
  where
  bytes = runPut $ do
    renderIP4Header hdr (S.length body)
    putByteString (S.take 8 body)

-- Message Handling ------------------------------------------------------------

-- | Deliver an ICMP message via the IP4 layer.
sendPacket :: Bool -> IP4 -> Icmp4Packet -> Icmp4 ()
sendPacket df dst pkt = do
  ip4 <- ip4Handle
  output $ IP4.sendIP4Packet ip4 df icmpProtocol dst
         $ runPutLazy
         $ renderIcmp4Packet pkt

-- | Handle incoming ICMP packets
handleIncoming :: IP4Header -> S.ByteString -> Icmp4 ()
handleIncoming hdr bs = do
  pkt <- liftRight (runGet parseIcmp4Packet bs)
  matchHandlers pkt
  case pkt of
    -- XXX: Only echo-request is handled at the moment
    Echo ident seqNum dat -> handleEchoRequest hdr ident seqNum dat
    _ty                   -> dropPacket


-- | Add an icmp packet handler.
handleAdd :: Handler -> Icmp4 ()
handleAdd k = do
  s <- get
  set s { icmpHandlers = k : icmpHandlers s }


-- | Respond to an echo request
handleEchoRequest :: IP4Header -> Identifier -> SequenceNumber -> S.ByteString
                  -> Icmp4 ()
handleEchoRequest hdr ident seqNum dat =
  sendPacket (ip4DontFragment hdr) (ip4SourceAddr hdr)
      (EchoReply ident seqNum dat)


-- | Output the IO actions for each handler that's registered.
matchHandlers :: Icmp4Packet -> Icmp4 ()
matchHandlers pkt = do
  s <- get
  output (mapM_ ($ pkt) (icmpHandlers s))
