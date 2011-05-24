{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hans.Message.Dhcp4Codec where

import Control.Applicative
import Data.List (find)
import qualified Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16, Word32)

import Hans.Address.IP4 (IP4,IP4Mask)
import Hans.Address.Mac (Mac)
import Hans.Address (Mask(..))

class CodecAtom a where
  getAtom :: Get a
  putAtom :: a -> Put
  atomSize :: a -> Int

instance (CodecAtom a, CodecAtom b) => CodecAtom (a,b) where
  getAtom       = (,) <$> getAtom <*> getAtom
  putAtom (a,b) = putAtom a *> putAtom b
  atomSize (a,b)= atomSize a + atomSize b

instance CodecAtom Word8 where
  getAtom       = getWord8
  putAtom n     = putWord8 n
  atomSize _    = 1

instance CodecAtom Word16 where
  getAtom       = getWord16be
  putAtom n     = putWord16be n
  atomSize _    = 2

instance CodecAtom Word32 where
  getAtom       = getWord32be
  putAtom n     = putWord32be n
  atomSize _    = 4

instance CodecAtom Bool where
  getAtom       = do b <- getWord8
                     case b of
                       0 -> return False
                       1 -> return True
                       _ -> fail "Expected 0/1 in boolean option"
  putAtom False = putWord8 0
  putAtom True  = putWord8 1
  atomSize _    = 1

instance CodecAtom IP4 where
  getAtom       = Data.Serialize.get
  putAtom       = Data.Serialize.put
  atomSize _    = 4

instance CodecAtom IP4Mask where
  getAtom       = withMask <$> getAtom <*> (unmask <$> getAtom)
  putAtom ip4mask = putAtom addr *> putAtom (SubnetMask mask)
    where (addr, mask) = getMaskComponents ip4mask
  atomSize _    = atomSize (undefined :: IP4)
                + atomSize (undefined :: SubnetMask)

instance CodecAtom Mac where
  getAtom       = Data.Serialize.get
  putAtom       = Data.Serialize.put
  atomSize _    = 6

-----------------------------------------------------------------------
-- Subnet parser/unparser operations ----------------------------------
-----------------------------------------------------------------------

newtype SubnetMask = SubnetMask { unmask :: Int}
  deriving (Show, Eq)

word32ToSubnetMask :: Word32 -> Maybe SubnetMask
word32ToSubnetMask mask =
  SubnetMask <$> find (\ i -> computeMask i == mask) [0..32]

subnetMaskToWord32 :: SubnetMask -> Word32
subnetMaskToWord32 (SubnetMask n) = computeMask n

computeMask :: Int -> Word32
computeMask n = 0-2^(32-n)

instance CodecAtom SubnetMask where
  getAtom       = do x <- getAtom
                     case word32ToSubnetMask x of
                       Just mask -> return mask
                       Nothing   -> fail "Invalid subnet mask"
  putAtom       = putAtom . subnetMaskToWord32
  atomSize _    = atomSize (undefined :: Word32)
