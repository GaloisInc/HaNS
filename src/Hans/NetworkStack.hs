{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hans.NetworkStack where

import Hans.Address (getMaskComponents)
import Hans.Address.IP4 (IP4Mask,IP4)
import Hans.Address.Mac (Mac)
import Hans.Channel (newChannel)
import Hans.Message.Ip4 (IP4Protocol)
import qualified Hans.Layer.Arp as Arp
import qualified Hans.Layer.Ethernet as Eth
import qualified Hans.Layer.Icmp4 as Icmp4
import qualified Hans.Layer.IP4 as IP4
import qualified Hans.Layer.Tcp as Tcp
import qualified Hans.Layer.Timer as Timer
import qualified Hans.Layer.Udp as Udp


-- Layer Accessors -------------------------------------------------------------

class HasArp stack where
  arpHandle :: stack -> Arp.ArpHandle

instance HasArp Arp.ArpHandle where arpHandle = id

class HasEthernet stack where
  ethernetHandle :: stack -> Eth.EthernetHandle

instance HasEthernet Eth.EthernetHandle where ethernetHandle = id

class HasIcmp4 stack where
  icmp4Handle :: stack -> Icmp4.Icmp4Handle

instance HasIcmp4 Icmp4.Icmp4Handle where icmp4Handle = id

class HasIP4 stack where
  ip4Handle :: stack -> IP4.IP4Handle

instance HasIP4 IP4.IP4Handle where ip4Handle = id

class HasTcp stack where
  tcpHandle :: stack -> Tcp.TcpHandle

instance HasTcp Tcp.TcpHandle where tcpHandle = id

class HasTimer stack where
  timerHandle :: stack -> Timer.TimerHandle

instance HasTimer Timer.TimerHandle where timerHandle = id

class HasUdp stack where
  udpHandle :: stack -> Udp.UdpHandle

instance HasUdp Udp.UdpHandle where udpHandle = id


-- Generic Network Stack -------------------------------------------------------

-- | An example implementation of the whole network stack.
data NetworkStack = NetworkStack
  { nsArp       :: Arp.ArpHandle
  , nsEthernet  :: Eth.EthernetHandle
  , nsIp4       :: IP4.IP4Handle
  , nsIcmp4     :: Icmp4.Icmp4Handle
  , nsTimers    :: Timer.TimerHandle
  , nsUdp       :: Udp.UdpHandle
  , nsTcp       :: Tcp.TcpHandle
  }

instance HasArp      NetworkStack where arpHandle      = nsArp
instance HasEthernet NetworkStack where ethernetHandle = nsEthernet
instance HasIP4      NetworkStack where ip4Handle      = nsIp4
instance HasIcmp4    NetworkStack where icmp4Handle    = nsIcmp4
instance HasTimer    NetworkStack where timerHandle    = nsTimers
instance HasTcp      NetworkStack where tcpHandle      = nsTcp
instance HasUdp      NetworkStack where udpHandle      = nsUdp

newNetworkStack :: IO NetworkStack
newNetworkStack  = do
  eth  <- newChannel
  arp  <- newChannel
  ip4  <- newChannel
  icmp <- newChannel
  th   <- newChannel
  udp  <- newChannel
  tcp  <- newChannel

  Timer.runTimerLayer  th
  Eth.runEthernetLayer eth
  Arp.runArpLayer      arp  eth th
  IP4.runIP4Layer      ip4  arp eth
  Icmp4.runIcmp4Layer  icmp ip4
  Udp.runUdpLayer      udp  ip4 icmp
  Tcp.runTcpLayer      tcp  ip4 th

  return NetworkStack
    { nsArp     = arp
    , nsEthernet= eth
    , nsIp4     = ip4
    , nsIcmp4   = icmp
    , nsTimers  = th
    , nsUdp     = udp
    , nsTcp     = tcp
    }


-- Ethernet Layer Interface ----------------------------------------------------

-- | Add an ethernet device to the ethernet layer.
addDevice :: HasEthernet stack => Mac -> Eth.Tx -> Eth.Rx -> stack -> IO ()
addDevice mac tx rx stack =
  Eth.addEthernetDevice (ethernetHandle stack) mac tx rx

-- | Remove a device from the ethernet layer.
removeDevice :: HasEthernet stack => Mac -> stack -> IO ()
removeDevice mac stack =
  Eth.removeEthernetDevice (ethernetHandle stack) mac

-- | Bring an ethernet device in the ethernet layer up.
deviceUp :: HasEthernet stack => Mac -> stack -> IO ()
deviceUp mac stack =
  Eth.startEthernetDevice (ethernetHandle stack) mac

-- | Bring an ethernet device in the ethernet layer down.
deviceDown :: HasEthernet stack => Mac -> stack -> IO ()
deviceDown mac stack =
  Eth.stopEthernetDevice (ethernetHandle stack) mac


-- IP4 Layer Interface ---------------------------------------------------------

type Mtu = Int

-- | Add an IP4 address to a network stack.
addIP4Addr :: (HasArp stack, HasIP4 stack)
           => IP4Mask -> Mac -> Mtu -> stack -> IO ()
addIP4Addr mask mac mtu stack = do
  let (addr,_) = getMaskComponents mask
  Arp.addLocalAddress (arpHandle stack) addr mac
  IP4.addIP4RoutingRule (ip4Handle stack) (IP4.Direct mask addr mtu)

-- | Add a route for a network, via an address.
routeVia :: HasIP4 stack => IP4Mask -> IP4 -> stack -> IO ()
routeVia mask addr stack =
  IP4.addIP4RoutingRule (ip4Handle stack) (IP4.Indirect mask addr)

-- | Register a handler for an IP4 protocol
listenIP4Protocol :: HasIP4 stack
                  => IP4Protocol -> IP4.Handler -> stack -> IO ()
listenIP4Protocol prot k stack = IP4.addIP4Handler (ip4Handle stack) prot k

-- | Register a handler for an IP4 protocol
ignoreIP4Protocol :: HasIP4 stack => IP4Protocol -> stack -> IO ()
ignoreIP4Protocol prot stack = IP4.removeIP4Handler (ip4Handle stack) prot
