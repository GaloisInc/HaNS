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
import Hans.Udp.State as Exports

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.BoundedChan (BoundedChan)
import qualified Data.ByteString as S
import           Data.IORef (IORef)


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
                                 }

instance HasConfig NetworkStack where
  getConfig = nsConfig
  {-# INLINE getConfig #-}

instance HasIP4State NetworkStack where
  getIP4State = nsIP4State
  {-# INLINE getIP4State #-}

instance HasUdpState NetworkStack where
  getUdpState = nsUdpState
  {-# INLINE getUdpState #-}

class HasNetworkStack a where
  getNetworkStack :: a -> NetworkStack

instance HasNetworkStack NetworkStack where
  getNetworkStack = id
  {-# INLINE getNetworkStack #-}

