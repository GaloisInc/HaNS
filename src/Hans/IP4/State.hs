{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.State (
    IP4State(..),
    SendSource(..),
    ResponderRequest(..),
    newIP4State,
    HasIP4State(..),
    addRoute,
    lookupRoute,
    isLocalAddr,
    nextIdent,
  ) where

import           Hans.Config (Config(..))
import           Hans.Device.Types (Device(..))
import           Hans.Ethernet (Mac)
import           Hans.IP4.ArpTable (ArpTable,newArpTable)
import           Hans.IP4.Fragments (FragTable,newFragTable)
import           Hans.IP4.Packet (IP4,IP4Protocol,IP4Ident)
import qualified Hans.IP4.RoutingTable as RT


import qualified Control.Concurrent.BoundedChan as BC
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           System.Random (StdGen,newStdGen,Random(random))


-- IP4 State -------------------------------------------------------------------

data SendSource = SourceAny
                  -- ^ Any interface that will route the message

                | SourceIP4 !IP4
                  -- ^ The interface with this address

                | SourceDev !Device !IP4
                  -- ^ Broadcast from this device


data ResponderRequest = Finish !Device !Mac [L.ByteString]
                        -- ^ Finish sending these IP4 packets

                      | Send !SendSource !IP4 !IP4Protocol L.ByteString
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

class HasIP4State state where
  getIP4State :: state -> IP4State

instance HasIP4State IP4State where
  getIP4State = id
  {-# INLINE getIP4State #-}


addRoute :: HasIP4State state => state -> Bool -> RT.Route -> IO ()
addRoute state = \ defRoute route ->
  atomicModifyIORef' ip4Routes (\ table -> (RT.addRule defRoute route table, ()))
  where
  IP4State { .. } = getIP4State state


-- | Lookup the source address, as well as the next hop and device.
lookupRoute :: HasIP4State state => state -> IP4 -> IO (Maybe (IP4,IP4,Device))
lookupRoute state = \ dest ->
  do routes <- readIORef ip4Routes
     case RT.lookupRoute dest routes of
       Just route -> return (Just ( RT.routeSource route
                                  , RT.routeNextHop dest route
                                  , RT.routeDevice route))
       Nothing    -> return Nothing
  where
  IP4State { .. } = getIP4State state


-- | Is this an address that's assigned to a device in the network stack?
isLocalAddr :: HasIP4State state => state -> IP4 -> IO (Maybe RT.Route)
isLocalAddr state = \ dst ->
  do rt <- readIORef ip4Routes
     return $! RT.isLocal dst rt
  where
  IP4State { .. } = getIP4State state


-- | Give back the result of using the 'random' function on the internal state.
nextIdent :: HasIP4State state => state -> IO IP4Ident
nextIdent state =
  atomicModifyIORef' ip4RandomSeed (\g -> case random g of (a,g') -> (g',a) )
  where
  IP4State { .. } = getIP4State state


-- | Give back routes that can be used to route the source of an IP packet.
routesFor :: HasIP4State state => state -> SendSource -> IO [RT.Route]
routesFor state src =
  case src of

    SourceAny ->
      do rt <- readIORef (ip4Routes (getIP4State state))
         return (RT.getRoutes rt)

    SourceIP4 addr ->
      do mb <- isLocalAddr state addr
         case mb of
           Just r  -> return [r]
           Nothing -> return []

    SourceDev dev addr ->
      do mb <- isLocalAddr state addr
         case mb of
           Just r | devName (RT.routeDevice r) == devName dev -> return [r]
           _                                                  -> return []
