module Hans.Layer.Arp.Table where

import Hans.Address.Mac
import Hans.Address.IP4

import Control.Arrow (second)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Map.Strict as Map


-- Arp Table -------------------------------------------------------------------

arpEntryTimeout :: POSIXTime
arpEntryTimeout = 60

data ArpEntry
  = ArpEntry    { arpMac     :: {-# UNPACK #-} !Mac
                , arpTimeout :: !POSIXTime
                }
  | ArpPending  { arpTimeout :: !POSIXTime
                }
 deriving Show

data ArpResult
  = KnownAddress {-# UNPACK #-} !Mac
  | Pending
  | Unknown

type ArpTable = Map.Map IP4 ArpEntry

stepArpTable :: POSIXTime -> ArpTable -> (ArpTable, [IP4])
stepArpTable now tab = second (Map.keys) (Map.partition p tab)
 where
  p ent = arpTimeout ent >= now

addArpEntry :: POSIXTime -> IP4 -> Mac -> ArpTable -> ArpTable
addArpEntry now ip mac = Map.insert ip ent where
  ent = ArpEntry
    { arpMac     = mac
    , arpTimeout = now + arpEntryTimeout
    }

-- | Assumption: there is not already a pending ARP query recorded in the
-- ARP table for the given IP address.
addPending :: POSIXTime -> IP4 -> ArpTable -> ArpTable
addPending now ip =
  Map.insert ip ArpPending
    { arpTimeout = now + arpEntryTimeout -- FIXME: should queries stay longer?
    }

-- | If the ARP table has a fully realized entry for the given IP address,
-- then return it. Otherwise return Pending if we're waiting for this info,
-- or Unknown if nothing is currently known about it.
lookupArpEntry :: IP4 -> ArpTable -> ArpResult
lookupArpEntry ip arp =
  case Map.lookup ip arp of
    Just (ArpEntry mac _) -> KnownAddress mac
    Just (ArpPending _)   -> Pending
    _                     -> Unknown
