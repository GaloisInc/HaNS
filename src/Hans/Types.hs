{-# LANGUAGE RecordWildCards #-}

module Hans.Types (
    module Hans.Types,
    module Exports
  ) where

import Hans.Config (Config,HasConfig(..))
import Hans.Device.Types (Device)
import Hans.IP4.ArpTable as Exports (ArpTable)
import Hans.IP4.Packet
import Hans.IP4.State as Exports
import Hans.IP4.RoutingTable as Exports (RoutingTable)
import Hans.Lens
import Hans.Tcp.State as Exports
import Hans.Udp.State as Exports

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.BoundedChan (BoundedChan)
import qualified Data.ByteString as S
import           Data.IORef (IORef,atomicModifyIORef',readIORef)


data InputPacket = FromDevice !Device !S.ByteString
                 | FromIP4 !Device !IP4Header !S.ByteString


data NetworkStack = NetworkStack { nsConfig :: !Config
                                   -- ^ The configuration for this instance of
                                   -- the network stack.

                                 , nsInput :: !(BoundedChan InputPacket)
                                   -- ^ The input packet queue

                                 , nsDevices :: {-# UNPACK #-} !(IORef [Device])
                                   -- ^ All registered devices

                                 , nsIP4State :: !IP4State
                                   -- ^ State for IP4 processing

                                 , nsIP4Responder :: !ThreadId
                                   -- ^ Internal IP4 responder

                                 , nsUdpState :: !UdpState
                                   -- ^ State for UDP processing

                                 , nsTcpState :: !TcpState
                                   -- ^ State for TCP processing

                                 , nsTcpTimers :: !ThreadId
                                   -- ^ The TCP timer thread

                                 , nsNameServers4 :: !(IORef [IP4])
                                 }

instance HasConfig NetworkStack where
  config = to nsConfig
  {-# INLINE config #-}

instance HasIP4State NetworkStack where
  ip4State = to nsIP4State
  {-# INLINE ip4State #-}

instance HasUdpState NetworkStack where
  udpState = to nsUdpState
  {-# INLINE udpState #-}

instance HasTcpState NetworkStack where
  tcpState = to nsTcpState
  {-# INLINE tcpState #-}


class HasNetworkStack ns where
  networkStack :: Getting r ns NetworkStack

instance HasNetworkStack NetworkStack where
  networkStack = id
  {-# INLINE networkStack #-}


addNameServer4 :: HasNetworkStack ns => ns -> IP4 -> IO ()
addNameServer4 ns addr =
  atomicModifyIORef' (nsNameServers4 (view networkStack ns))
      (\addrs -> (addr:addrs,()))

getNameServers4 :: HasNetworkStack ns => ns -> IO [IP4]
getNameServers4 ns = readIORef (nsNameServers4 (view networkStack ns))
