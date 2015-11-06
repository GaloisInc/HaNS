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

import Hans.Arp (newArpState)
import Hans.Config
import Hans.Device
import Hans.IP4.Types (newIP4State)
import Hans.Input
import Hans.Types

import Control.Concurrent.BoundedChan (newBoundedChan)
import Data.IORef (newIORef,atomicModifyIORef')


-- | Create a network stack with no devices registered.
newNetworkStack :: Config -> IO NetworkStack
newNetworkStack nsConfig =
  do nsInput    <- newBoundedChan (cfgInputQueueSize nsConfig)
     nsDevices  <- newIORef []
     nsArpState <- newArpState nsConfig
     nsIP4State <- newIP4State
     return NetworkStack { .. }

-- | Initialize and register a device with the network stack.
-- NOTE: this does not start the device.
addDevice :: DeviceName -> DeviceConfig -> NetworkStack -> IO Device
addDevice devName devConfig NetworkStack { .. } =
  do dev <- openDevice devName devConfig nsInput
     atomicModifyIORef' nsDevices (\devs -> (dev : devs, ()))
     return dev
