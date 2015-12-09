{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hans.Addr (
    Addr(), sameFamily,
    NetworkAddr(..)
  ) where

import qualified Hans.IP4.Packet as IP4

import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)


data Addr = Addr4 IP4.IP4
            deriving (Eq,Ord,Show,Generic,Typeable)

instance Hashable Addr

sameFamily :: Addr -> Addr -> Bool
sameFamily Addr4{} Addr4{} = True


class (Hashable addr, Show addr, Typeable addr, Eq addr, Generic addr)
  => NetworkAddr addr where
  -- | Forget what kind of address this is.
  toAddr :: addr -> Addr

  -- | Try to remember what this opaque address was.
  fromAddr :: Addr -> Maybe addr

  -- | Check to see if this address is the wildcard address.
  isWildcardAddr :: addr -> Bool

  -- | The wildcard address
  wildcardAddr :: addr -> addr

  -- | Check to see if this address is the broadcast address.
  isBroadcastAddr :: addr -> Bool

  -- | The broadcast address.
  broadcastAddr :: addr -> addr


instance NetworkAddr Addr where
  toAddr                       = id
  fromAddr addr                = Just addr
  isWildcardAddr  (Addr4 addr) = isWildcardAddr addr
  wildcardAddr    (Addr4 addr) = Addr4 (wildcardAddr addr)
  isBroadcastAddr (Addr4 addr) = isBroadcastAddr addr
  broadcastAddr   (Addr4 addr) = Addr4 (broadcastAddr addr)
  {-# INLINE toAddr #-}
  {-# INLINE fromAddr #-}
  {-# INLINE isWildcardAddr #-}
  {-# INLINE wildcardAddr #-}
  {-# INLINE isBroadcastAddr #-}
  {-# INLINE broadcastAddr #-}


instance NetworkAddr IP4.IP4 where
  toAddr                           = Addr4
  fromAddr (Addr4 addr)            = Just addr
  isWildcardAddr IP4.WildcardIP4   = True
  isWildcardAddr _                 = False
  wildcardAddr _                   = IP4.WildcardIP4
  isBroadcastAddr IP4.BroadcastIP4 = True
  isBroadcastAddr _                = False
  broadcastAddr _                  = IP4.BroadcastIP4
  {-# INLINE toAddr #-}
  {-# INLINE fromAddr #-}
  {-# INLINE isWildcardAddr #-}
  {-# INLINE wildcardAddr #-}
  {-# INLINE isBroadcastAddr #-}
  {-# INLINE broadcastAddr #-}
