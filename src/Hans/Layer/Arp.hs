{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Hans.Layer.Arp (
    ArpHandle
  , runArpLayer

    -- External Interface
  , arpWhoHas
  , arpIP4Packet
  , addLocalAddress
  ) where

import Hans.Address.IP4
import Hans.Address.Mac
import Hans.Channel
import Hans.Layer
import Hans.Layer.Arp.Table
import Hans.Layer.Ethernet
import Hans.Layer.Timer
import Hans.Message.Arp
import Hans.Message.EthernetFrame
import Hans.Utils

import Control.Concurrent (forkIO,takeMVar,putMVar,newEmptyMVar)
import Control.Monad (forM_,mplus,guard,unless,when)
import Data.Serialize (decode,encode)
import MonadLib (BaseM(inBase),set,get)
import qualified Data.Map as Map

-- | A handle to a running arp layer.
type ArpHandle = Channel (Arp ())


-- | Start an arp layer.
runArpLayer :: ArpHandle -> EthernetHandle -> TimerHandle -> IO ()
runArpLayer h eth th = do
  addEthernetHandler eth (EtherType 0x0806) (send h . handleIncoming)
  let i = emptyArpState h eth th
  void (forkIO (loopLayer i (receive h) id))


-- | Lookup the hardware address associated with an IP address.
arpWhoHas :: BaseM m IO => ArpHandle -> IP4 -> m (Maybe Mac)
arpWhoHas h !ip = inBase $ do
  var <- newEmptyMVar
  send h (whoHas ip (putMVar var))
  takeMVar var


-- | Send an IP packet via the arp layer, to resolve the underlying hardware
-- addresses.
arpIP4Packet :: ArpHandle -> IP4 -> IP4 -> Packet -> IO ()
arpIP4Packet h !src !dst !pkt = send h (handleOutgoing src dst pkt)


addLocalAddress :: ArpHandle -> IP4 -> Mac -> IO ()
addLocalAddress h !ip !mac = send h (handleAddAddress ip mac)


-- Message Handling ------------------------------------------------------------

type Arp = Layer ArpState

data ArpState = ArpState
  { arpTable    :: ArpTable
  , arpAddrs    :: Map.Map IP4 Mac -- this layer's addresses
  , arpWaiting  :: Map.Map IP4 [Maybe Mac -> IO ()]
  , arpEthernet :: EthernetHandle
  , arpTimers   :: TimerHandle
  , arpSelf     :: ArpHandle
  }

emptyArpState :: ArpHandle -> EthernetHandle -> TimerHandle -> ArpState
emptyArpState h eth ts = ArpState
  { arpTable    = Map.empty
  , arpAddrs    = Map.empty
  , arpWaiting  = Map.empty
  , arpEthernet = eth
  , arpTimers   = ts
  , arpSelf     = h
  }

ethernetHandle :: Arp EthernetHandle
ethernetHandle  = arpEthernet `fmap` get

timerHandle :: Arp TimerHandle
timerHandle  = arpTimers `fmap` get

addEntry :: IP4 -> Mac -> Arp ()
addEntry spa sha = do
  state <- get
  now   <- time
  let table' = addArpEntry now spa sha (arpTable state)
  table' `seq` set state { arpTable = table' }
  runWaiting spa (Just sha)

addWaiter :: IP4 -> (Maybe Mac -> IO ()) -> Arp ()
addWaiter addr cont = do
  state <- get
  set state { arpWaiting = Map.alter f addr (arpWaiting state) }
 where
  f Nothing   = Just [cont]
  f (Just ks) = Just (cont:ks)

runWaiting :: IP4 -> Maybe Mac -> Arp ()
runWaiting spa sha = do
  state <- get
  let (mb,waiting') = Map.updateLookupWithKey f spa (arpWaiting state)
        where f _ _ = Nothing
  -- run the callbacks associated with this protocol address
  let run cb = output (cb sha)
  mapM_ run (maybe [] reverse mb)
  waiting' `seq` set state { arpWaiting = waiting' }

updateExistingEntry :: IP4 -> Mac -> Arp Bool
updateExistingEntry spa sha = do
  state <- get
  let update = do
        guard (spa `Map.member` arpTable state)
        addEntry spa sha
        return True
  update `mplus` return False

localHwAddress :: IP4 -> Arp Mac
localHwAddress pa = do
  state <- get
  just (Map.lookup pa (arpAddrs state))

sendArpPacket :: ArpPacket Mac IP4 -> Arp ()
sendArpPacket msg = do
  eth <- ethernetHandle
  let frame = EthernetFrame
        { etherSource = arpSHA msg
        , etherDest   = arpTHA msg
        , etherType   = 0x0806
        , etherData   = encode msg
        }
  output (sendEthernet eth frame)

advanceArpTable :: Arp ()
advanceArpTable  = do
  now   <- time
  state <- get
  let (table', timedOut) = stepArpTable now (arpTable state)
  set state { arpTable = table' }
  forM_ timedOut $ \ x -> runWaiting x Nothing

-- | Handle a who-has request
whoHas :: IP4 -> (Maybe Mac -> IO ()) -> Arp ()
whoHas ip k = (k' =<< localHwAddress ip) `mplus` query
  where
  k' addr = output (k (Just addr))

  query = do
    advanceArpTable
    state  <- get
    case lookupArpEntry ip (arpTable state) of
      KnownAddress mac    -> k' mac
      Pending             -> addWaiter ip k
      Unknown             -> do
        let addrs = Map.toList (arpAddrs state)
            msg (spa,sha) = ArpPacket
              { arpHwType = 0x1
              , arpPType  = 0x0800
              , arpSHA    = sha
              , arpSPA    = spa
              , arpTHA    = Mac 0xff 0xff 0xff 0xff 0xff 0xff
              , arpTPA    = ip
              , arpOper   = ArpRequest
              }
        now <- time
        let table' = addPending now ip (arpTable state)
        set state { arpTable = table' }
        addWaiter ip k
        mapM_ (sendArpPacket . msg) addrs
        th <- timerHandle
        output (delay th 10000 (send (arpSelf state) advanceArpTable))

-- Message Handling ------------------------------------------------------------

-- | Process an incoming arp packet
handleIncoming :: Packet -> Arp ()
handleIncoming bs = do
  msg <- liftRight (decode bs)
  -- ?Do I have the hardware type in ar$hrd
  -- Yes: (This check is enforced by the type system)
  --   [optionally check the hardware length ar$hln]
  --   ?Do I speak the protocol in ar$pro?
  --   Yes: (This check is also enforced by the type system)
  --     [optionally check the protocol length ar$pln]
  --     Merge_flag := false
  --     If the pair <protocol type, sender protocol address> is
  --         already in my translation table, update the sender
  --         hardware address field of the entry with the new
  --         information in the packet and set Merge_flag to true. 
  let sha = arpSHA msg
  let spa = arpSPA msg
  merge <- updateExistingEntry spa sha
  --     ?Am I the target protocol address?
  let tpa = arpTPA msg
  lha <- localHwAddress tpa
  --     Yes:
  --       If Merge_flag is false, add the triplet <protocol type,
  --           sender protocol address, sender hardware address> to
  --           the translation table.
  unless merge (addEntry spa sha)
  --       ?Is the opcode ares_op$REQUEST?  (NOW look at the opcode!!)
  --       Yes:
  when (arpOper msg == ArpRequest) $ do
  --           Swap hardware and protocol fields, putting the local
  --               hardware and protocol addresses in the sender fields.
    let msg' = msg { arpSHA = lha , arpSPA = tpa
                   , arpTHA = sha , arpTPA = spa
  --           Set the ar$op field to ares_op$REPLY
                   , arpOper = ArpReply }
  --           Send the packet to the (new) target hardware address on
  --               the same hardware on which the request was received.
    sendArpPacket msg'


-- | Handle a request to associate an ip with a mac address for a local device
handleAddAddress :: IP4 -> Mac -> Arp ()
handleAddAddress ip mac = do
  state <- get
  let addrs' = Map.insert ip mac (arpAddrs state)
  addrs' `seq` set state { arpAddrs = addrs' }


-- | Output a packet to the ethernet layer.
handleOutgoing :: IP4 -> IP4 -> Packet -> Arp ()
handleOutgoing src dst bs = do
  eth <- ethernetHandle
  lha <- localHwAddress src
  let frame dha = EthernetFrame
        { etherDest   = dha
        , etherSource = lha
        , etherType   = 0x0800
        , etherData   = bs
        }
  whoHas dst $ \ res -> case res of
    Nothing  -> return ()
    Just dha -> sendEthernet eth (frame dha)
