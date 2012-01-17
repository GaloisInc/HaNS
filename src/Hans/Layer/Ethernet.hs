{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Hans.Layer.Ethernet (
    EthernetHandle
  , runEthernetLayer

    -- * External Interface
  , Tx
  , Rx
  , sendEthernet
  , queueEthernet
  , addEthernetDevice
  , removeEthernetDevice
  , addEthernetHandler
  , removeEthernetHandler
  , startEthernetDevice
  , stopEthernetDevice
  ) where

import Hans.Address.Mac
import Hans.Channel
import Hans.Layer
import Hans.Message.EthernetFrame
import Hans.Utils (void,just)

import Control.Concurrent (forkIO,ThreadId,killThread)
import Control.Monad (mplus)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPutLazy)
import MonadLib (get,set)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map             as Map
import qualified Data.ByteString      as S


-- Ethernet Layer --------------------------------------------------------------

type Handler = S.ByteString -> IO ()

type Tx = L.ByteString   -> IO ()
type Rx = EthernetHandle -> IO ()

type EthernetHandle = Channel (Eth ())

-- | Run the ethernet layer.
runEthernetLayer :: EthernetHandle -> IO ()
runEthernetLayer h =
  void (forkIO (loopLayer (emptyEthernetState h) (receive h) id))


-- External Interface ----------------------------------------------------------

sendEthernet :: EthernetHandle -> EthernetFrame -> L.ByteString -> IO ()
sendEthernet h !frame body = send h (handleOutgoing frame body)

queueEthernet :: EthernetHandle -> S.ByteString -> IO ()
queueEthernet h !pkt = send h (handleIncoming pkt)

startEthernetDevice :: EthernetHandle -> Mac -> IO ()
startEthernetDevice h !m = send h (startDevice m)

stopEthernetDevice :: EthernetHandle -> Mac -> IO ()
stopEthernetDevice h !m = send h (stopDevice m)

addEthernetDevice :: EthernetHandle -> Mac -> Tx -> Rx -> IO ()
addEthernetDevice h !mac tx rx = send h (addDevice mac tx rx)

removeEthernetDevice :: EthernetHandle -> Mac -> IO ()
removeEthernetDevice h !mac = send h (delDevice mac)

addEthernetHandler :: EthernetHandle -> EtherType -> Handler -> IO ()
addEthernetHandler h !et k = send h (addHandler et k)

removeEthernetHandler :: EthernetHandle -> EtherType -> IO ()
removeEthernetHandler h !et = send h (removeHandler et)


-- Ethernet Message Monad ------------------------------------------------------

data EthernetDevice = EthernetDevice
  { devTx :: Tx
  , devRx :: IO ()
  , devUp :: Maybe ThreadId
  }

emptyDevice :: Tx -> IO () -> EthernetDevice
emptyDevice tx rx = EthernetDevice
  { devTx = tx
  , devRx = rx
  , devUp = Nothing
  }


type Eth = Layer EthernetState

data EthernetState = EthernetState
  { ethHandlers :: Handlers EtherType Handler
  , ethDevices  :: Map.Map Mac EthernetDevice
  , ethHandle   :: EthernetHandle
  }

instance ProvidesHandlers EthernetState EtherType Handler where
  getHandlers      = ethHandlers
  setHandlers hs i = i { ethHandlers = hs }

emptyEthernetState :: EthernetHandle -> EthernetState
emptyEthernetState h = EthernetState
  { ethHandlers = emptyHandlers
  , ethDevices  = Map.empty
  , ethHandle   = h
  }

self :: Eth EthernetHandle
self = ethHandle `fmap` get


-- Message Handling ------------------------------------------------------------

-- | Handle an incoming packet, from a device.
handleIncoming :: S.ByteString -> Eth ()
handleIncoming pkt = do
  (hdr,body) <- liftRight (runGet parseEthernetFrame pkt)
  h          <- getHandler (etherType hdr)
  output (h body)


-- | Get the device associated with a mac address.
getDevice :: Mac -> Eth EthernetDevice
getDevice mac = do
  state <- get
  just (Map.lookup mac (ethDevices state))


-- | Set the device associated with a mac address.
setDevice :: Mac -> EthernetDevice -> Eth ()
setDevice mac dev = do
  state <- get
  let ds' = Map.insert mac dev (ethDevices state)
  ds' `seq` set state { ethDevices = ds' }


-- | Send an outgoing ethernet frame via the device that it's associated with.
handleOutgoing :: EthernetFrame -> L.ByteString -> Eth ()
handleOutgoing frame body = do
  dev <- getDevice (etherSource frame)
  output (devTx dev (runPutLazy (renderEthernetFrame frame body)))


-- | Add an ethernet device to the state.
addDevice :: Mac -> Tx -> Rx -> Eth ()
addDevice mac tx rx = do
  stopDevice mac `mplus` return ()
  h <- self
  setDevice mac (emptyDevice tx (rx h))


-- | Remove a device
delDevice :: Mac -> Eth ()
delDevice mac = do
  stopDevice mac
  state <- get
  let ds' = Map.delete mac (ethDevices state)
  ds' `seq` set state { ethDevices = ds' }


-- | Stop an ethernet device.
stopDevice :: Mac -> Eth ()
stopDevice mac = do
  dev <- getDevice mac
  case devUp dev of
    Nothing  -> return ()
    Just tid -> do
      output (killThread tid)
      setDevice mac dev { devUp = Nothing }


-- | Start an ethernet device.
startDevice :: Mac -> Eth ()
startDevice mac = do
  dev <- getDevice mac
  case devUp dev of
    Just _  -> return ()
    -- XXX: add functionality to pipe the threadid back into the layer state.
    Nothing -> output (void (forkIO (devRx dev)))
               --setDevice mac dev { devUp = Just tid }
