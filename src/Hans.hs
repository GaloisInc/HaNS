{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

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
    IP4.Route(..), IP4.RouteType(Direct,Indirect),
    addIP4Route,
    lookupIP4Route,

  ) where

import           Hans.Config
import           Hans.Device
import           Hans.Device.Loopback
import qualified Hans.IP4.State as IP4
import qualified Hans.IP4.Packet as IP4
import qualified Hans.IP4.RoutingTable as IP4 (Route(..),RouteType(..))
import qualified Hans.IP4.Output as IP4 (responder)
import           Hans.Input
import           Hans.Types
import qualified Hans.Udp.State as Udp

import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan (newBoundedChan)
import Data.IORef (newIORef,atomicModifyIORef')


-- | Create a network stack with no devices registered.
newNetworkStack :: Config -> IO NetworkStack
newNetworkStack nsConfig =
  do nsInput        <- newBoundedChan (cfgInputQueueSize nsConfig)
     nsDevices      <- newIORef []
     nsIP4State     <- IP4.newIP4State nsConfig
     nsUdpState     <- Udp.newUdpState nsConfig
     nsNameServers4 <- newIORef []

     rec nsIP4Responder <- forkIO (IP4.responder ns)
         let ns = NetworkStack { .. }

     registerLoopback ns

     return ns

-- | Create and register the loopback device. Additionally, add its routing
-- information.
registerLoopback :: NetworkStack -> IO ()
registerLoopback ns =
  do lo <- newLoopbackDevice ns
     atomicModifyIORef' (nsDevices ns) (\devs -> (lo : devs, ()))

     -- add the route for 127.0.0.0/8
     addIP4Route ns False
         IP4.Route { routeNetwork = IP4.IP4Mask (IP4.packIP4 127 0 0 1) 8
                   , routeType    = IP4.Direct
                   , routeDevice  = lo }

-- | Initialize and register a device with the network stack.
-- NOTE: this does not start the device.
addDevice :: NetworkStack -> DeviceName -> DeviceConfig -> IO Device
addDevice ns devName devConfig =
  do dev <- openDevice ns devName devConfig
     atomicModifyIORef' (nsDevices ns) (\devs -> (dev : devs, ()))
     return dev

-- | Add a route to the IP4 layer.
addIP4Route :: NetworkStack -> Bool -> IP4.Route -> IO ()
addIP4Route NetworkStack { .. } = IP4.addRoute nsIP4State
{-# INLINE addIP4Route #-}

lookupIP4Route :: NetworkStack -> IP4.IP4 -> IO (Maybe (IP4.IP4,IP4.IP4,Device))
lookupIP4Route NetworkStack { .. } = IP4.lookupRoute nsIP4State
{-# INLINE lookupIP4Route #-}
