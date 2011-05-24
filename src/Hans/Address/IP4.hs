{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Address.IP4 where

import Hans.Address
import Hans.Utils (Endo)

import Control.Monad (guard,liftM2)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getWord32be)
import Data.Serialize.Put (putWord32be)
import Data.Bits (Bits((.&.),(.|.),shiftL,shiftR))
import Data.Data (Data)
import Data.List (intersperse)
import Data.Typeable (Typeable)
import Data.Word (Word8,Word32)
import Numeric (readDec)


data IP4 = IP4
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  deriving (Ord,Eq,Typeable,Data)

instance Address IP4 where
  addrSize _ = 4

  toBits (IP4 a b c d) = f 0x80 a (f 0x80 b (f 0x80 c (f 0x80 d [])))
    where
    f 0 _ xs = xs
    f m i xs = (i .&. m == 0) : f (m `shiftR` 1) i xs

instance Serialize IP4 where
  get = do
    n <- getWord32be
    return $! convertFromWord32 n
  put ip = putWord32be (convertToWord32 ip)

instance Show IP4 where
  showsPrec _ (IP4 a b c d) = foldl (.) id
                            $ intersperse (showChar '.')
                              [shows a, shows b, shows c, shows d]

instance Read IP4 where
  readsPrec _ rest0 = do
    (a, '.':rest1) <- readDec rest0
    (b, '.':rest2) <- readDec rest1
    (c, '.':rest3) <- readDec rest2
    (d,     rest4) <- readDec rest3
    return (IP4 a b c d, rest4)

convertToWord32 :: IP4 -> Word32
convertToWord32 (IP4 a b c d)
  = fromIntegral a `shiftL` 24
  + fromIntegral b `shiftL` 16
  + fromIntegral c `shiftL` 8
  + fromIntegral d

convertFromWord32 :: Word32 -> IP4
convertFromWord32 n = IP4 a b c d
  where
  a = fromIntegral (n `shiftR` 24)
  b = fromIntegral (n `shiftR` 16)
  c = fromIntegral (n `shiftR` 8)
  d = fromIntegral n


data IP4Mask = IP4Mask
  {-# UNPACK #-} !IP4
  {-# UNPACK #-} !Word8
  deriving (Eq,Ord,Typeable,Data,Show)

instance Serialize IP4Mask where
  put (IP4Mask i m) = put i >> put m
  get = liftM2 IP4Mask get get

instance Read IP4Mask where
  readsPrec x rest0 = do
    (addr,'/':rest1) <- readsPrec x rest0
    (bits,    rest2) <- readsPrec x rest1
    guard (bits >= 0 && bits <= 32)
    return (IP4Mask addr bits, rest2)

instance Mask IP4Mask IP4 where
  masksAddress mask@(IP4Mask _ bits) a2 =
    clearHostBits mask == clearHostBits (IP4Mask a2 bits)
  getMaskRange x = (clearHostBits x, setHostBits x)
  withMask addr bits = IP4Mask addr (fromIntegral bits)
  getMaskComponents (IP4Mask addr bits) = (addr,fromIntegral bits)
  broadcastAddress = setHostBits

modifyAsWord32 :: Endo Word32 -> Endo IP4
modifyAsWord32 f = convertFromWord32 . f . convertToWord32

clearHostBits :: IP4Mask -> IP4
clearHostBits (IP4Mask addr bits) = modifyAsWord32 (.&. mask) addr
  where mask = -2 ^ (32 - bits)

setHostBits :: IP4Mask -> IP4
setHostBits (IP4Mask addr bits) = modifyAsWord32 (.|. mask) addr
  where mask = 2 ^ (32 - bits) - 1
