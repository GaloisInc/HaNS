{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}

module Hans.Setup where

import Hans.Address
import Hans.Address.IP4
import Hans.Address.Mac
import Hans.Channel
import Hans.Layer.Arp
import Hans.Layer.Ethernet
import Hans.Layer.IP4
import Hans.Layer.Icmp4
import Hans.Layer.Tcp
import Hans.Layer.Timer
import Hans.Layer.Udp


data NetworkStack = NetworkStack
  { nsArp       :: ArpHandle
  , nsEthernet  :: EthernetHandle
  , nsIp4       :: IP4Handle
  , nsIcmp4     :: Icmp4Handle
  , nsTimers    :: TimerHandle
  , nsUdp       :: UdpHandle
  , nsTcp       :: TcpHandle
  }


setup :: IO NetworkStack
setup  = do
  eth  <- newChannel
  arp  <- newChannel
  ip4  <- newChannel
  icmp <- newChannel
  th   <- newChannel
  udp  <- newChannel
  tcp  <- newChannel

  runTimerLayer    th
  runEthernetLayer eth
  runArpLayer      arp  eth th
  runIP4Layer      ip4  arp eth
  runIcmp4Layer    icmp ip4
  runUdpLayer      udp  ip4 icmp
  runTcpLayer      tcp  ip4 th

  return NetworkStack
    { nsArp     = arp
    , nsEthernet= eth
    , nsIp4     = ip4
    , nsIcmp4   = icmp
    , nsTimers  = th
    , nsUdp     = udp
    , nsTcp     = tcp
    }


data SomeOption = forall o. Option o => SomeOption o

instance Show SomeOption where
  showsPrec p (SomeOption o) = parens (showsPrec 11 o)
    where
    parens body | p > 10    = showString "(SomeOption " . body . showChar ')'
                | otherwise = showString "SomeOption " . body

toOption :: Option o => o -> SomeOption
toOption  = SomeOption

class Show o => Option o where
  apply :: o -> NetworkStack -> IO ()


instance Option SomeOption where
  apply (SomeOption o) ns = apply o ns

instance Option o => Option [o] where
  apply os ns = mapM_ (`apply` ns) os


data OptEthernet mask = LocalEthernet mask Mac
    deriving Show

instance Option (OptEthernet IP4Mask) where
  apply (LocalEthernet mask mac) ns = do
    let (addr,_) = getMaskComponents mask
    addLocalAddress (nsArp ns) addr mac
    addIP4RoutingRule (nsIp4 ns) (Direct mask addr 1500)


data OptRoute mask addr = Route mask addr
    deriving Show

instance Option (OptRoute IP4Mask IP4) where
  apply (Route mask addr) ns = addIP4RoutingRule (nsIp4 ns) (Indirect mask addr)
