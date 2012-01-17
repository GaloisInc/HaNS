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
module Hans.Utils.Checksum(
         computeChecksum
       , finalizeChecksum
       , computePartialChecksum
       , computePartialChecksumLazy
       , clearChecksum
       , pokeChecksum
       )
 where

import Control.Exception (assert)
import Data.Bits (Bits(shiftL,shiftR,complement,clearBit,(.&.),rotate))
import Data.List (foldl')
import Data.Word (Word8,Word16,Word32)
import Foreign.Storable (pokeByteOff)
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString        as S
import qualified Data.ByteString.Unsafe as S


-- | Clear the two bytes at the checksum offset of a rendered packet.
clearChecksum :: S.ByteString -> Int -> IO S.ByteString
clearChecksum b off = S.unsafeUseAsCStringLen b $ \(ptr,len) -> do
  assert (len > off + 1) (pokeByteOff ptr off (0 :: Word16))
  return b

-- | Poke a checksum into a bytestring.
pokeChecksum :: Word16 -> S.ByteString -> Int -> IO S.ByteString
pokeChecksum cs b off = S.unsafeUseAsCStringLen b $ \(ptr,len) -> do
  assert (off < len + 1) (pokeByteOff ptr off (rotate cs 8))
  return b

finalizeChecksum :: Word32 -> Word16
finalizeChecksum  = complement . fromIntegral . fold32 . fold32

-- | Compute the final checksum, using the given initial value.
computeChecksum :: Word32 -> S.ByteString -> Word16
computeChecksum c0 = finalizeChecksum . computePartialChecksum c0

-- | Compute the checksum of a lazy bytestring.
computePartialChecksumLazy :: Word32 -> L.ByteString -> Word32
computePartialChecksumLazy c0 = foldl' computePartialChecksum c0 . L.toChunks

-- | Compute a partial checksum, yielding a value suitable to be passed to
-- computeChecksum.
computePartialChecksum :: Word32 -> S.ByteString -> Word32
computePartialChecksum base b = result
 where
  !n' = S.length b

  !result
    | odd n'    = step most hi 0
    | otherwise = most
    where hi    = S.unsafeIndex b (n'-1)

  !most         = loop (fromIntegral base) 0

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
