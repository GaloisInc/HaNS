{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |A module providing checksum computations to other parts of Hans. The
-- checksum here is the standard Internet 16-bit checksum (the one's 
-- complement of the one's complement sum of the data).

module Hans.Checksum(
    -- * Checksums
    computeChecksum,
    Checksum(..),
    PartialChecksum(),
    emptyPartialChecksum,
    finalizeChecksum,
    stepChecksum,
  ) where

import           Data.Bits (Bits(shiftL,shiftR,complement,clearBit,(.&.)))
import           Data.List (foldl')
import           Data.Word (Word8,Word16,Word32)
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Short  as Sh
import qualified Data.ByteString.Unsafe as S


data PartialChecksum = PartialChecksum { pcAccum :: {-# UNPACK #-} !Word32
                                       , pcCarry ::                !(Maybe Word8)
                                       } deriving (Eq,Show)

emptyPartialChecksum :: PartialChecksum
emptyPartialChecksum  = PartialChecksum
  { pcAccum = 0
  , pcCarry = Nothing
  }

finalizeChecksum :: PartialChecksum -> Word16
finalizeChecksum pc = complement (fromIntegral (fold32 (fold32 result)))
  where
  fold32 :: Word32 -> Word32
  fold32 x = (x .&. 0xFFFF) + (x `shiftR` 16)

  result = case pcCarry pc of
    Nothing   -> pcAccum pc
    Just prev -> stepChecksum (pcAccum pc) prev 0
{-# INLINE finalizeChecksum #-}


computeChecksum :: Checksum a => a -> Word16
computeChecksum a = finalizeChecksum (extendChecksum a emptyPartialChecksum)
{-# INLINE computeChecksum #-}

-- | Incremental checksum computation interface.
class Checksum a where
  extendChecksum :: a -> PartialChecksum -> PartialChecksum

instance Checksum a => Checksum [a] where
  extendChecksum as = \pc -> foldl' (flip extendChecksum) pc as
  {-# INLINE extendChecksum #-}

instance Checksum L.ByteString where
  extendChecksum lbs = \pc -> extendChecksum (L.toChunks lbs) pc
  {-# INLINE extendChecksum #-}

-- XXX this could be faster if we could mirror the structure of the instance for
-- S.ByteString
instance Checksum Sh.ShortByteString where
  extendChecksum shb = \ pc -> extendChecksum (Sh.fromShort shb) pc


instance Checksum S.ByteString where
  extendChecksum b pc
    | S.null b  = pc
    | otherwise = case pcCarry pc of
        Nothing   -> result
        Just prev -> extendChecksum (S.tail b) PartialChecksum
          { pcCarry = Nothing
          , pcAccum = stepChecksum (pcAccum pc) prev (S.unsafeIndex b 0)
          }
    where

    n' = S.length b
    n  = clearBit n' 0 -- aligned to two

    result = PartialChecksum
      { pcAccum = loop (pcAccum pc) 0
      , pcCarry = carry
      }

    carry
      | odd n'    = Just $! S.unsafeIndex b n
      | otherwise = Nothing

    loop !acc off
      | off < n   = loop (stepChecksum acc hi lo) (off + 2)
      | otherwise = acc
      where hi    = S.unsafeIndex b off
            lo    = S.unsafeIndex b (off+1)

stepChecksum :: Word32 -> Word8 -> Word8 -> Word32
stepChecksum acc hi lo = acc + fromIntegral hi `shiftL` 8 + fromIntegral lo
{-# INLINE stepChecksum #-}
