module Hans.Types where

import Hans.Arp
import Hans.Config
import Hans.Queue
import Hans.Device

import Control.Concurrent.STM (TVar)


data NetworkStack = NetworkStack { nsInput :: !(Queue InputPacket)
                                   -- ^ The input packet queue

                                 , nsDevices :: {-# UNPACK #-} !(TVar [Device])
                                   -- ^ All registered devices

                                 , nsArpState :: !ArpState
                                   -- ^ State for Arp processing
                                 }
