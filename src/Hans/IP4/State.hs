{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.State (
    IP4State(..), ResponderRequest(..),
    newIP4State,
    addRoute,
    lookupRoute,
    nextRandom
  ) where

import           Hans.Config (Config(..))
import           Hans.Device (Device)
import           Hans.Ethernet (Mac,EtherType)
import           Hans.IP4.ArpTable (ArpTable,newArpTable)
import           Hans.IP4.Fragments (FragTable,newFragTable)
import           Hans.IP4.Icmp4 (Icmp4Packet)
import           Hans.IP4.Packet (IP4,IP4Protocol)
import qualified Hans.IP4.RoutingTable as RT

import           Control.Concurrent (ThreadId,forkIO)
import qualified Control.Concurrent.BoundedChan as BC
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           System.Random (StdGen,newStdGen,Random(random))


-- IP4 State -------------------------------------------------------------------

data ResponderRequest = Finish !Device !Mac [L.ByteString]
                        -- ^ Finish sending these IP4 packets

                      | Send !IP4 !IP4Protocol L.ByteString
                       -- ^ Send this IP4 payload to this address


data IP4State = IP4State { ip4Routes :: !(IORef RT.RoutingTable)
                           -- ^ Addresses currently assigned to devices.

                         , ip4ArpTable :: !ArpTable
                           -- ^ The ARP cache.

                         , ip4Fragments :: !FragTable
                           -- ^ IP4 packet fragments

                         , ip4ArpRetry :: {-# UNPACK #-} !Int
                           -- ^ Arp retry count

                         , ip4ArpRetryDelay :: {-# UNPACK #-} !Int

                         , ip4ResponderQueue :: !(BC.BoundedChan ResponderRequest)

                         , ip4RandomSeed :: !(IORef StdGen)
                         }

newIP4State :: Config -> IO IP4State
newIP4State cfg =
  do ip4Routes         <- newIORef RT.empty
     ip4ArpTable       <- newArpTable cfg
     ip4Fragments      <- newFragTable cfg
     ip4ResponderQueue <- BC.newBoundedChan 32
     ip4RandomSeed     <- newIORef =<< newStdGen
     return IP4State { ip4ArpRetry      = cfgArpRetry cfg
                     , ip4ArpRetryDelay = cfgArpRetryDelay cfg * 1000
                     , .. }

addRoute :: IP4State -> Bool -> RT.Route -> IO ()
addRoute IP4State { .. } defRoute route =
  atomicModifyIORef' ip4Routes (\ table -> (RT.addRule defRoute route table, ()))

-- | Lookup the source address, as well as the next hop and device.
lookupRoute :: IP4State -> IP4 -> IO (Maybe (IP4,IP4,Device))
lookupRoute IP4State { .. } dest =
  do routes <- readIORef ip4Routes
     case RT.lookupRoute dest routes of
       Just route -> return (Just ( RT.routeSource route
                                  , RT.routeNextHop dest route
                                  , RT.routeDevice route))
       Nothing    -> return Nothing


-- | Give back the result of using the 'random' function on the internal state.
nextRandom :: Random a => IP4State -> IO a
nextRandom IP4State { .. } =
  atomicModifyIORef' ip4RandomSeed (\g -> case random g of (a,g') -> (g',a) )
