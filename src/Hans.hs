{-# LANGUAGE RecordWildCards #-}

module Hans (
    -- * Network Stack
    NetworkStack(), NetworkStackConfig(..), defaultNetworkStackConfig,
    newNetworkStack,

    -- * Devices
    DeviceName,
    Device(), DeviceConfig(..), defaultDeviceConfig,
    addDevice,
    listDevices,
    closeDevice

  ) where

import Hans.Device
import Hans.Input
import Hans.Queue
import Hans.Types

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically,newTVarIO,modifyTVar')


-- | Create a network stack with no devices registered.
newNetworkStack :: NetworkStackConfig -> IO NetworkStack
newNetworkStack NetworkStackConfig { .. } =
  do nsInput        <- newQueue nscInputQueueSize
     nsDevices      <- newTVarIO []
     nsPacketThread <- forkIO (processPackets nsInput)
     return NetworkStack { .. }

-- | Initialize and register a device with the network stack.
addDevice :: DeviceName -> DeviceConfig -> NetworkStack -> IO Device
addDevice devName devConfig NetworkStack { .. } =
  do dev <- openDevice devName devConfig nsInput
     atomically (modifyTVar' nsDevices (dev :))
     return dev
