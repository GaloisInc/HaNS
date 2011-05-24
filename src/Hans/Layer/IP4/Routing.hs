module Hans.Layer.IP4.Routing (
    -- * Routing Rules
    Rule(..)
  , Mtu

    -- * Routing Table
  , RoutingTable
  , emptyRoutingTable
  , addRule
  , route
  , localAddrs
  , sourceMask
  ) where

import Data.PrefixTree as PT
import Hans.Address.IP4
import Hans.Address

import Data.Maybe (mapMaybe)


-- Routing Rules ---------------------------------------------------------------

type Mtu = Int

data Rule mask addr
  = Direct   mask addr Mtu
  | Indirect mask addr
  deriving Show


-- Routing Table ---------------------------------------------------------------

type RoutingTable addr = PrefixTree (Dest addr)

data Dest addr
  = NextHop addr
  | Via addr Mtu
  deriving Show


emptyRoutingTable :: Address addr => RoutingTable addr
emptyRoutingTable  = PT.empty


{-# SPECIALIZE addRule :: Rule IP4Mask IP4 -> RoutingTable IP4
                       -> RoutingTable IP4 #-}
-- | Add a rule to the routing table.
addRule :: Mask mask addr
        => Rule mask addr -> RoutingTable addr -> RoutingTable addr
addRule rule table = case rule of
  Direct   mask addr mtu -> k mask (Via addr mtu)
  Indirect mask addr     -> k mask (NextHop addr)
  where
  k m d = insert ks d table
    where
    (addr,bits) = getMaskComponents m
    ks          = take bits (toBits addr)


{-# SPECIALIZE route :: IP4 -> RoutingTable IP4 -> Maybe (IP4,IP4,Mtu) #-}
-- | Discover the source and destination when trying to route an address.
route :: Address addr => addr -> RoutingTable addr -> Maybe (addr,addr,Mtu)
route addr t = do
  r <- match (toBits addr) t
  case r of
    Via s mtu   -> return (s,addr,mtu)
    NextHop hop -> do
      Via s mtu <- match (toBits hop) t
      return (s,hop,mtu)


{-# SPECIALIZE sourceMask :: IP4 -> RoutingTable IP4 -> Maybe IP4Mask #-}
-- | Find the mask that would be used to route an address.
sourceMask :: Mask mask addr => addr -> RoutingTable addr -> Maybe mask
sourceMask addr table = do
  src <- key (toBits addr) table
  let bits = fromIntegral (addrSize addr * 8) - length src
  return (addr `withMask` bits)


-- | Dump all local addresses.
localAddrs :: Address addr => RoutingTable addr -> [addr]
localAddrs table = mapMaybe p (PT.elems table)
  where
  p (Via s _) = Just s
  p _         = Nothing
