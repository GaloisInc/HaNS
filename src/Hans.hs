{-# LANGUAGE RecordWildCards #-}

module Hans (
    -- * Network Stack
    NetworkStack(), Config(..), defaultConfig,
    newNetworkStack,
    processPackets,

    -- * Devices
    DeviceName,
    Device(), DeviceConfig(..), defaultDeviceConfig,
    addDevice,
    listDevices,
    closeDevice,
    startDevice

  ) where

import Hans.Arp
import Hans.Config
import Hans.Device
import Hans.Input
import Hans.Queue
import Hans.Types

import Control.Concurrent.STM (atomically,newTVarIO,modifyTVar')


-- | Create a network stack with no devices registered.
newNetworkStack :: Config -> IO NetworkStack
newNetworkStack cfg =
  do nsInput    <- newQueue (cfgInputQueueSize cfg)
     nsDevices  <- newTVarIO []
     nsArpState <- newArpState cfg
     return NetworkStack { .. }

-- | Initialize and register a device with the network stack.
addDevice :: DeviceName -> DeviceConfig -> NetworkStack -> IO Device
addDevice devName devConfig NetworkStack { .. } =
  do dev <- openDevice devName devConfig nsInput
     atomically (modifyTVar' nsDevices (dev :))
     return dev
