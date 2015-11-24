{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.RoutingTable (
    Route(..), RouteType(..),
    routeSource, routeNextHop,
    RoutingTable,
    empty,
    addRule,
    deleteRule,
    lookupRoute,
    isLocal,
    getRoutes,
  ) where

import Hans.Device.Types (Device)
import Hans.IP4.Packet

import Control.Monad (guard)
import Data.Bits ((.&.))
import Data.List (insertBy)
import Data.Word (Word32)


data RouteType = Direct
               | Indirect !IP4
               | Loopback

data Route = Route { routeNetwork :: {-# UNPACK #-} !IP4Mask
                   , routeType    ::                !RouteType
                   , routeDevice  ::                !Device
                   }

routeSource :: Route -> IP4
routeSource Route { routeNetwork = IP4Mask addr _ } = addr

routeNextHop :: IP4 -> Route -> IP4
routeNextHop dest route =
  case routeType route of
    Direct           -> dest
    Indirect nextHop -> nextHop
    Loopback         -> routeSource route


data Rule = Rule { ruleMask   :: {-# UNPACK #-} !Word32
                 , rulePrefix :: {-# UNPACK #-} !Word32
                 , ruleRoute  ::                !Route
                 }

ruleMaskLen :: Rule -> Int
ruleMaskLen rule = maskBits (routeNetwork (ruleRoute rule))

ruleSource :: Rule -> IP4
ruleSource rule = maskAddr (routeNetwork (ruleRoute rule))

ruleDevice :: Rule -> Device
ruleDevice rule = routeDevice (ruleRoute rule)

mkRule :: Route -> Rule
mkRule ruleRoute = Rule { .. }
  where
  IP4Mask (IP4 w) bits = routeNetwork ruleRoute

  ruleMask             = netmask bits
  rulePrefix           = ruleMask .&. w

routesTo :: Rule -> IP4 -> Bool
routesTo Rule { .. } (IP4 addr) = addr .&. ruleMask == rulePrefix

-- | Simple routing.
data RoutingTable = RoutingTable { rtRules :: [Rule]
                                   -- ^ Insertions must keep this list ordered
                                   -- by the network prefix length, descending.

                                 , rtDefault :: !(Maybe Route)
                                   -- ^ Optional default route.
                                 }

empty :: RoutingTable
empty  = RoutingTable { rtRules = [], rtDefault = Nothing }

getRoutes :: RoutingTable -> [Route]
getRoutes RoutingTable { .. } = map ruleRoute rtRules

addRule :: Bool -> Route -> RoutingTable -> RoutingTable
addRule isDefault route RoutingTable { .. } =
  rule `seq`
    RoutingTable { rtRules   = insertBy maskLenDesc rule rtRules
                 , rtDefault = if isDefault
                                  then Just route
                                  else rtDefault
                 }

  where

  -- compare b to a, to get descending order
  maskLenDesc a b = compare (ruleMaskLen b) (ruleMaskLen a)

  rule = mkRule route

deleteRule :: IP4Mask -> RoutingTable -> RoutingTable
deleteRule mask RoutingTable { .. } =
  rules' `seq` def' `seq` RoutingTable { rtRules = rules', rtDefault = def' }

  where

  rules' =
    do rule <- rtRules
       guard (routeNetwork (ruleRoute rule) /= mask)
       return rule

  def' =
    case rtDefault of
      Just Route { .. } | routeNetwork == mask -> Nothing
      _                                        -> rtDefault

lookupRoute :: IP4 -> RoutingTable -> Maybe Route
lookupRoute dest RoutingTable { .. } = foldr findRoute rtDefault rtRules
  where
  findRoute rule continue
    | rule `routesTo` dest = Just (ruleRoute rule)
    | otherwise            = continue

-- | If the address given is the source address for a rule in the table, return
-- the associated 'Device'.
isLocal :: IP4 -> RoutingTable -> Maybe Route
isLocal addr RoutingTable { .. } = foldr hasSource Nothing rtRules
  where
  hasSource rule continue
    | ruleSource rule == addr = Just (ruleRoute rule)
    | otherwise               = continue
