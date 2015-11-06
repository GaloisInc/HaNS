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
    computeChecksumLazy,
    computePartialChecksumLazy,

    -- ** Incremental interface
    PartialChecksum(),
    emptyPartialChecksum,
    computePartialChecksum,
    finalizeChecksum
  ) where

import Data.Bits (Bits(shiftL,shiftR,complement,clearBit,(.&.)))
import Data.List (foldl')
import Data.Word (Word8,Word16,Word32)
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString        as S
import qualified Data.ByteString.Unsafe as S


data PartialChecksum = PartialChecksum
  { pcAccum :: {-# UNPACK #-} !Word32
  , pcCarry :: !(Maybe Word8)
  }

emptyPartialChecksum :: PartialChecksum
emptyPartialChecksum  = PartialChecksum
  { pcAccum = 0
  , pcCarry = Nothing
  }

finalizeChecksum :: PartialChecksum -> Word16
finalizeChecksum pc = complement (fromIntegral (fold32 (fold32 result)))
  where
  result = case pcCarry pc of
    Nothing   -> pcAccum pc
    Just prev -> step (pcAccum pc) prev 0

-- | Compute the final checksum, using the given initial value.
computeChecksum :: Word32 -> S.ByteString -> Word16
computeChecksum c0 bs = finalizeChecksum (computePartialChecksum pc bs)
  where
  pc = PartialChecksum { pcAccum = c0, pcCarry = Nothing }

computeChecksumLazy :: Word32 -> L.ByteString -> Word16
computeChecksumLazy c0 lbs =
  finalizeChecksum (computePartialChecksumLazy pc lbs)
  where
  pc = PartialChecksum { pcAccum = c0, pcCarry = Nothing }

-- | Compute the checksum of a lazy bytestring.
computePartialChecksumLazy :: PartialChecksum -> L.ByteString -> PartialChecksum
computePartialChecksumLazy c0 = foldl' computePartialChecksum c0 . L.toChunks

-- | Compute a partial checksum, yielding a value suitable to be passed to
-- computeChecksum.
computePartialChecksum :: PartialChecksum -> S.ByteString -> PartialChecksum
computePartialChecksum pc b
  | S.null b  = pc
  | otherwise = case pcCarry pc of
      Nothing   -> result
      Just prev -> computePartialChecksum (pc
        { pcCarry = Nothing
        , pcAccum = step (pcAccum pc) prev (S.unsafeIndex b 0)
        }) (S.tail b)
  where

  !n' = S.length b

  result = PartialChecksum
    { pcAccum = loop (fromIntegral (pcAccum pc)) 0
    , pcCarry = carry
    }

  carry
    | odd n'    = Just (S.unsafeIndex b (n' - 1))
    | otherwise = Nothing

  loop !acc off
    | off < n   = loop (step acc hi lo) (off + 2)
    | otherwise = acc
    where hi    = S.unsafeIndex b off
          lo    = S.unsafeIndex b (off+1)
          n     = clearBit n' 0

step :: Word32 -> Word8 -> Word8 -> Word32
step acc hi lo = acc + fromIntegral hi `shiftL` 8 + fromIntegral lo

fold32 :: Word32 -> Word32
fold32 x = (x .&. 0xFFFF) + (x `shiftR` 16)
