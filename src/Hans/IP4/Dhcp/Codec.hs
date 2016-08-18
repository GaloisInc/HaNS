{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Dhcp.Codec where

import Hans.Addr (IP4,IP4Mask,pattern IP4Mask,getIP4,putIP4,maskAddr,maskBits)
import Hans.Ethernet (Mac,getMac,putMac)

import Data.List (find)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16, Word32)


class CodecAtom a where
  getAtom :: Get a
  putAtom :: a -> Put
  atomSize :: a -> Int

instance (CodecAtom a, CodecAtom b) => CodecAtom (a,b) where
  getAtom       = do a <- getAtom
                     b <- getAtom
                     return (a,b)
  putAtom (a,b) = do putAtom a
                     putAtom b
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
  getAtom       = getIP4
  putAtom       = putIP4
  atomSize _    = 4

instance CodecAtom IP4Mask where
  getAtom =
    do addr            <- getAtom
       SubnetMask mask <- getAtom
       return $! IP4Mask addr mask

  putAtom m =
    do putAtom (maskAddr m)
       putAtom (SubnetMask (maskBits m))

  atomSize _    = atomSize (undefined :: IP4)
                + atomSize (undefined :: SubnetMask)

instance CodecAtom Mac where
  getAtom       = getMac
  putAtom       = putMac
  atomSize _    = 6

-----------------------------------------------------------------------
-- Subnet parser/unparser operations ----------------------------------
-----------------------------------------------------------------------

newtype SubnetMask = SubnetMask { unmask :: Int
                                } deriving (Show, Eq)

word32ToSubnetMask :: Word32 -> Maybe SubnetMask
word32ToSubnetMask mask =
  do i <- find (\ i -> computeMask i == mask) [0..32]
     return (SubnetMask i)

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
