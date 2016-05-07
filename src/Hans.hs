{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}

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

    -- * Network Layer
    Addr(), sameFamily,
    NetworkAddr(..),
    Network(..),
    RouteInfo(..),

    -- ** IP4
    IP4.IP4(), IP4.packIP4, IP4.unpackIP4,
    IP4.IP4Mask(..),
    IP4.Route(..), IP4.RouteType(Direct,Indirect),
    addIP4Route,

  ) where

import           Hans.Addr (NetworkAddr(..),Addr(),sameFamily)
import           Hans.Config
import           Hans.Device
import           Hans.Device.Loopback
import qualified Hans.IP4.State as IP4
import qualified Hans.IP4.Packet as IP4
import qualified Hans.IP4.RoutingTable as IP4 (Route(..),RouteType(..))
import qualified Hans.IP4.Output as IP4 (responder)
import           Hans.Input
import           Hans.Network
import           Hans.Threads (forkNamed)
import           Hans.Types
import qualified Hans.Tcp.Output as Tcp (responder)
import qualified Hans.Tcp.Timers as Tcp
import qualified Hans.Udp.Output as Udp (responder)

import Control.Concurrent.BoundedChan (newBoundedChan)
import Data.IORef (newIORef,atomicModifyIORef')

#ifdef HANS_TARGET_XEN
import Hypervisor.XenStore (XenStore)
#endif


-- | Create a network stack with no devices registered.
newNetworkStack :: Config -> IO NetworkStack
newNetworkStack nsConfig =
  do nsInput        <- newBoundedChan (cfgInputQueueSize nsConfig)
     nsNat          <- newNatState nsConfig
     nsDevices      <- newIORef []
     nsIP4State     <- newIP4State nsConfig
     nsUdpState     <- newUdpState nsConfig
     nsTcpState     <- newTcpState nsConfig
     nsNameServers4 <- newIORef []

     rec nsIP4Responder <- forkNamed "IP4.responder" (IP4.responder ns)
         nsTcpTimers    <- forkNamed "Tcp.tcpTimers" (Tcp.tcpTimers ns)
         nsTcpResponder <- forkNamed "Tcp.responder" (Tcp.responder ns)
         nsUdpResponder <- forkNamed "Udp.responder" (Udp.responder ns)
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

#ifdef HANS_TARGET_XEN
addDevice :: XenStore -> NetworkStack -> DeviceName -> DeviceConfig -> IO Device
addDevice xs ns devName devConfig =
  do dev <- openDevice xs ns devName devConfig
     atomicModifyIORef' (nsDevices ns) (\devs -> (dev : devs, ()))
     return dev
#else
addDevice :: NetworkStack -> DeviceName -> DeviceConfig -> IO Device
addDevice ns devName devConfig =
  do dev <- openDevice ns devName devConfig
     atomicModifyIORef' (nsDevices ns) (\devs -> (dev : devs, ()))
     return dev
#endif

-- | Add a route to the IP4 layer.
addIP4Route :: NetworkStack -> Bool -> IP4.Route -> IO ()
addIP4Route NetworkStack { .. } = IP4.addRoute nsIP4State
{-# INLINE addIP4Route #-}
