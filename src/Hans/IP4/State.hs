{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.State (
    IP4State(..),
    newIP4State
  ) where

import Hans.Config (Config)
import Hans.Device (Device)
import Hans.IP4.ArpTable (ArpTable,newArpTable,purgeArpTable)
import Hans.IP4.Packet (IP4)

import Control.Concurrent (ThreadId,forkIO)
import Data.IORef (IORef,newIORef)


-- IP4 State -------------------------------------------------------------------

type DevAddrs = [(IP4,Device)]

data IP4State = IP4State { ip4Addrs :: !(IORef DevAddrs)
                           -- ^ Addresses currently assigned to devices.

                         , ip4ArpTable :: !ArpTable
                           -- ^ The ARP cache.

                         , ip4ArpPurgeThread :: !ThreadId
                           -- ^ The 'ThreadId' of the thread that periodically
                           -- clears out the ARP cache.

                         }

newIP4State :: Config -> IO IP4State
newIP4State cfg =
  do ip4Addrs          <- newIORef []
     ip4ArpTable       <- newArpTable cfg
     ip4ArpPurgeThread <- forkIO (purgeArpTable cfg ip4ArpTable)
     return IP4State { .. }
