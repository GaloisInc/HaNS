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
    startDevice,

    -- * IP4
    IP4.IP4(), IP4.packIP4,
    IP4.IP4Mask(..),
    IP4.Route(..),
    addIP4Route,
    lookupIP4Route,

  ) where

import           Hans.Config
import           Hans.Device
import qualified Hans.IP4.State as IP4
import qualified Hans.IP4.Packet as IP4
import qualified Hans.IP4.RoutingTable as IP4
import           Hans.Input
import           Hans.Types

import Control.Concurrent.BoundedChan (newBoundedChan)
import Data.IORef (newIORef,atomicModifyIORef')


-- | Create a network stack with no devices registered.
newNetworkStack :: Config -> IO NetworkStack
newNetworkStack nsConfig =
  do nsInput    <- newBoundedChan (cfgInputQueueSize nsConfig)
     nsDevices  <- newIORef []
     nsIP4State <- IP4.newIP4State nsConfig
     return NetworkStack { .. }

-- | Initialize and register a device with the network stack.
-- NOTE: this does not start the device.
addDevice :: DeviceName -> DeviceConfig -> NetworkStack -> IO Device
addDevice devName devConfig NetworkStack { .. } =
  do dev <- openDevice devName devConfig nsInput
     atomicModifyIORef' nsDevices (\devs -> (dev : devs, ()))
     return dev

-- | Add a route to the IP4 layer.
addIP4Route :: NetworkStack -> Bool -> IP4.Route -> IO ()
addIP4Route NetworkStack { .. } = IP4.addRoute nsIP4State
{-# INLINE addIP4Route #-}

lookupIP4Route :: NetworkStack -> IP4.IP4 -> IO (Maybe (IP4.IP4,Device))
lookupIP4Route NetworkStack { .. } = IP4.lookupRoute nsIP4State
{-# INLINE lookupIP4Route #-}
