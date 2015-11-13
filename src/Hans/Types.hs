module Hans.Types where

import Hans.Config (Config)
import Hans.Device (Device,InputPacket)
import Hans.IP4.State (IP4State)

import Control.Concurrent (ThreadId)
import Control.Concurrent.BoundedChan (BoundedChan)
import Data.IORef (IORef)


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
                                 }
