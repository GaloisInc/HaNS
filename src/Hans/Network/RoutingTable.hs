{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Network.RoutingTable (
    Route(..), RouteType(..),
    routeSource, routeNextHop,
    RoutingTable,
    empty,
    addRule,
    deleteRule,
    lookupRoute,
    isLocal,
    getRoutes,
    routesForDev,
  ) where

import Hans.Addr
import Hans.Device.Types (Device)

import Control.Monad (guard)
import Data.List (insertBy)
import Data.Maybe (mapMaybe)


data RouteType addr = Direct
                    | Indirect !addr
                    | Loopback

data (Eq mask, IsMask mask addr) => Route addr mask = Route {
         routeNetwork :: !mask
       , routeType    :: !(RouteType addr)
       , routeDevice  :: !Device
       }

routeSource :: IsMask mask addr => Route addr mask -> addr
routeSource Route { .. } = maskAddr routeNetwork

routeNextHop :: IsMask mask addr => addr -> Route addr mask -> addr
routeNextHop dest route =
  case routeType route of
    Direct           -> dest
    Indirect nextHop -> nextHop
    Loopback         -> routeSource route


data Rule addr mask = Rule { ruleTest  :: !(addr -> Bool)
                           , ruleRoute :: !(Route addr mask)
                           }

ruleMaskLen :: IsMask mask addr => Rule addr mask -> Int
ruleMaskLen rule = maskBits (routeNetwork (ruleRoute rule))

ruleSource :: IsMask mask addr => Rule addr mask -> addr
ruleSource rule = maskAddr (routeNetwork (ruleRoute rule))

ruleDevice :: IsMask mask addr => Rule addr mask -> Device
ruleDevice rule = routeDevice (ruleRoute rule)

mkRule :: IsMask mask addr => Route addr mask -> Rule addr mask
mkRule ruleRoute = Rule { ruleTest = isMember (routeNetwork ruleRoute), .. }

routesTo :: Rule addr mask -> addr -> Bool
routesTo Rule { .. } = ruleTest

-- | Simple routing.
data RoutingTable addr mask = RoutingTable {
         rtRules :: [Rule addr mask]
         -- ^ Insertions must keep this list ordered
         -- by the network prefix length, descending.

       , rtDefault :: !(Maybe (Route addr mask))
         -- ^ Optional default route.
       }

empty :: RoutingTable addr mask
empty  = RoutingTable { rtRules = [], rtDefault = Nothing }

getRoutes :: RoutingTable addr mask -> [Route addr mask]
getRoutes RoutingTable { .. } = map ruleRoute rtRules

addRule :: IsMask mask addr =>
           Bool -> Route addr mask -> RoutingTable addr mask ->
           RoutingTable addr mask
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

deleteRule :: IsMask mask addr =>
              mask -> RoutingTable addr mask ->
              RoutingTable addr mask
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

lookupRoute :: addr -> RoutingTable addr mask -> Maybe (Route addr mask)
lookupRoute dest RoutingTable { .. } = foldr findRoute rtDefault rtRules
  where
  findRoute rule continue
    | rule `routesTo` dest = Just (ruleRoute rule)
    | otherwise            = continue

-- | If the address given is the source address for a rule in the table, return
-- the associated 'Device'.
isLocal :: IsMask mask addr =>
           addr -> RoutingTable addr mask ->
           Maybe (Route addr mask)
isLocal addr RoutingTable { .. } = foldr hasSource Nothing rtRules
  where
  hasSource rule continue
    | ruleSource rule == addr = Just (ruleRoute rule)
    | otherwise               = continue

-- | Give back routes that involve this device.
routesForDev :: IsMask mask addr =>
                Device -> RoutingTable addr mask ->
                [Route addr mask]
routesForDev dev RoutingTable { .. } = mapMaybe usesDev rtRules
  where
  usesDev rule | ruleDevice rule == dev = Just (ruleRoute rule)
               | otherwise              = Nothing
