module Hans.Types where

import Hans.Arp
import Hans.Config
import Hans.Device

import Control.Concurrent.BoundedChan (BoundedChan)
import Data.IORef (IORef)


data NetworkStack = NetworkStack { nsConfig :: !Config
                                   -- ^ The configuration for this instance of
                                   -- the network stack.

                                 , nsInput :: !(BoundedChan InputPacket)
                                   -- ^ The input packet queue

                                 , nsDevices :: {-# UNPACK #-} !(IORef [Device])
                                   -- ^ All registered devices

                                 , nsArpState :: !ArpState
                                   -- ^ State for Arp processing
                                 }
