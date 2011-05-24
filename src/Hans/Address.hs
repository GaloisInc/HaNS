{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}

module Hans.Address where

import Data.Serialize (Serialize)
import Data.Word (Word8)

class (Ord a, Serialize a) => Address a where
  addrSize :: a -> Word8
  toBits   :: a -> [Bool]


class Address addr => Mask mask addr | addr -> mask, mask -> addr where
  masksAddress      :: mask -> addr -> Bool
  withMask          :: addr -> Int -> mask
  getMaskComponents :: mask -> (addr,Int)
  getMaskRange      :: mask -> (addr,addr)
  broadcastAddress  :: mask -> addr


isBroadcast :: (Eq addr, Mask mask addr) => mask -> addr -> Bool
isBroadcast m a = broadcastAddress m == a
