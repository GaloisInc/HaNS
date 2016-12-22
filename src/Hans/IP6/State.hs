{-# LANGUAGE RecordWildCards #-}
module Hans.IP6.State(
         IP6State(..)
       , newIP6State
       , HasIP6State(..)
       , addRoute
       )
 where

import           Data.IORef(IORef,newIORef,atomicModifyIORef',readIORef)
import           Hans.Addr(IP6,IP6Mask)
import           Hans.Config(Config(..))
import           Hans.Device.Types(Device(..))
import           Hans.IP4.Fragments(FragTable,newFragTable)
import           Hans.IP6.Packet(IP6Ident)
import qualified Hans.Network.RoutingTable as RT
import           Hans.Lens
import           System.Random(StdGen,newStdGen,Random(random))

data IP6State = IP6State { ip6Routes     :: !(IORef (RT.RoutingTable IP6 IP6Mask))
                         , ip6Fragments  :: !FragTable
                         , ip6RandomSeed :: !(IORef StdGen)
                         }

newIP6State :: Config -> IO IP6State
newIP6State cfg =
  do ip6Routes     <- newIORef RT.empty
     ip6Fragments  <- newFragTable cfg
     ip6RandomSeed <- newIORef =<< newStdGen -- XXX: Replace with something from config
     return IP6State{ .. }

class HasIP6State state where
  ip6State :: Getting r state IP6State

instance HasIP6State IP6State where
  ip6State = id
  {-# INLINE ip6State #-}

addRoute :: HasIP6State state => state -> Bool -> RT.Route IP6 IP6Mask -> IO ()
addRoute state = \ defRoute route ->
  atomicModifyIORef' ip6Routes (\ table -> (RT.addRule defRoute route table,()))
 where IP6State {..} = view ip6State state

lookupRoute6 :: HasIP6State state => state -> IP6 -> IO (Maybe (IP6,IP6,Device))
lookupRoute6 state = \ dest ->
  do routes <- readIORef ip6Routes
     case RT.lookupRoute dest routes of
       Just route -> return (Just ( RT.routeSource route
                                  , RT.routeNextHop dest route
                                  , RT.routeDevice route ))
       Nothing    -> return Nothing
 where IP6State{..} = view ip6State state

-- | Is this an address that's assigned to a device in the network stack?
isLocalAddr :: HasIP6State state =>
               state -> IP6 ->
               IO (Maybe (RT.Route IP6 IP6Mask))
isLocalAddr state = \ dst ->
  do rt <- readIORef ip6Routes
     return $! RT.isLocal dst rt
 where
  IP6State { .. } = view ip6State state

-- | Give back the result of using the 'random' function on the internal state.
nextIdent :: HasIP6State state => state -> IO IP6Ident
nextIdent state =
  atomicModifyIORef' ip6RandomSeed (\g -> case random g of (a,g') -> (g',a) )
 where IP6State { .. } = view ip6State state

-- | Give back the list of routing rules associated with this device.
routesForDev :: HasIP6State state =>
                state -> Device ->
                IO [RT.Route IP6 IP6Mask]
routesForDev state dev =
  do routes <- readIORef (ip6Routes (view ip6State state))
     return $! RT.routesForDev dev routes
