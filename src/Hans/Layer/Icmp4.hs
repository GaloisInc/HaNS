{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Hans.Layer.Icmp4 (
    Icmp4Handle
  , runIcmp4Layer
  , addIcmp4Handler
  ) where

import Hans.Address.IP4 (IP4)
import Hans.Channel
import Hans.Layer
import Hans.Message.Icmp4
    (Icmp4Packet(Echo,EchoReply),parseIcmp4Packet,renderIcmp4Packet
    ,Identifier,SequenceNumber)
import Hans.Message.Ip4
import Hans.Utils
import qualified Hans.Layer.IP4 as IP4

import Control.Concurrent (forkIO)
import Data.Serialize (runGet,runPutLazy)
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
    $ \src dst bs -> send h (handleIncoming src dst bs)
  void (forkIO (loopLayer handles (receive h) id))

data Icmp4Handles = Icmp4Handles
  { icmpIp4      :: IP4.IP4Handle
  , icmpHandlers :: [Handler]
  }

type Icmp4 = Layer Icmp4Handles

ip4Handle :: Icmp4 IP4.IP4Handle
ip4Handle  = icmpIp4 `fmap` get

sendPacket :: IP4 -> Icmp4Packet -> Icmp4 ()
sendPacket dst pkt = do
  ip4 <- ip4Handle
  output $ IP4.sendIP4Packet ip4 icmpProtocol dst
         $ runPutLazy
         $ renderIcmp4Packet pkt

-- | Add a handler for Icmp4 messages that match the provided predicate.
addIcmp4Handler :: Icmp4Handle -> Handler -> IO ()
addIcmp4Handler h k = send h (handleAdd k)

-- Message Handling ------------------------------------------------------------

-- | Handle incoming ICMP packets
handleIncoming :: IP4 -> IP4 -> S.ByteString -> Icmp4 ()
handleIncoming src _dst bs = do
  pkt <- liftRight (runGet parseIcmp4Packet bs)
  matchHandlers pkt
  case pkt of
    -- XXX: Only echo-request is handled at the moment
    Echo ident seqNum dat -> handleEchoRequest src ident seqNum dat
    _ty         -> do
      --output (putStrLn ("Unhandled ICMP message type: " ++ show ty))
      dropPacket


-- | Add an icmp packet handler.
handleAdd :: Handler -> Icmp4 ()
handleAdd k = do
  s <- get
  set s { icmpHandlers = k : icmpHandlers s }


-- | Respond to an echo request
handleEchoRequest :: IP4 -> Identifier -> SequenceNumber -> S.ByteString
                  -> Icmp4 ()
handleEchoRequest src ident seqNum dat = do
  sendPacket src (EchoReply ident seqNum dat)


-- | Output the IO actions for each handler that's registered.
matchHandlers :: Icmp4Packet -> Icmp4 ()
matchHandlers pkt = do
  s <- get
  output (mapM_ ($ pkt) (icmpHandlers s))
