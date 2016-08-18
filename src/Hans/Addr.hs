{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}

module Hans.Addr.Types (
  -- * Addresses

  -- ** IPv6
  IP6(),

  -- ** IPv4 Address
  IP4, pattern Addr4,
  getIP4, putIP4,
  packIP4, unpackIP4,
  showIP4, readIP4,
  pattern BroadcastIP4,
  pattern WildcardIP4,
  pattern CurrentNetworkIP4,

  -- * Masks
  IsMask(..),

  -- ** IPv6
  IP6Mask(),

  -- ** IPv4
  IP4Mask(),
  broadcastAddress,
  readIP4Mask, showIP4Mask,

  ) where

import Hans.Lens (view,byte,set)

import Data.Bits ((.&.),(.|.),shiftR,bit,complement)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize(..),Putter,Get,getWord32be,putWord32be)
import Data.Typeable (Typeable)
import Data.Word (Word8,Word32,Word64)
import GHC.Generics (Generic)
import Numeric (readDec)


-- IPv6 Addresses --------------------------------------------------------------

-- | Generic IP4/IP6 address type.
data IP6 = IP6 {-# UNPACK #-} !Word64
               {-# UNPACK #-} !Word64
           deriving (Eq,Ord,Show,Read,Generic,Typeable)

instance Hashable IP6


data IP6Mask = IP6Mask {-# UNPACK #-} !IP6
                       {-# UNPACK #-} !Int
               deriving (Ord,Show,Read,Generic,Typeable)

instance Eq IP6Mask where
  IP6Mask a b == IP6Mask c d = b == d && a == c

instance Hashable IP6Mask


-- IPv4 Addresses --------------------------------------------------------------

isIPv4 :: IP6 -> Maybe IP4
isIPv4 addr@(IP6 0x0 w64) | w64 `shiftR` 32 == 0xffff = Just (IP4 addr)
isIPv4 _                                              = Nothing

pattern Addr4 addr <- (isIPv4 -> Just addr)
  where
  Addr4 (IP4 addr) = addr


newtype IP4 = IP4 IP6
              deriving (Eq,Ord,Show,Read,Generic,Typeable)

instance Serialize IP4 where
  get = getIP4
  put = putIP4
  {-# INLINE get #-}
  {-# INLINE put #-}

pattern BroadcastIP4      = IP4 (IP6 0x0 0xffffffffffff)
pattern CurrentNetworkIP4 = IP4 (IP6 0x0 0xffff00000000)
pattern WildcardIP4       = IP4 (IP6 0x0 0xffff00000000)


fromIP4 :: Word32 -> IP6
fromIP4 w32 = IP6 0x0 (0xffff00000000 .|. fromIntegral w32)
{-# INLINE fromIP4 #-}

unsafeToIP4 :: IP6 -> Word32
unsafeToIP4 (IP6 _ w64) = fromIntegral w64
{-# INLINE unsafeToIP4 #-}

getIP4 :: Get IP4
getIP4  = do w <- getWord32be
             return (IP4 (fromIP4 w))
{-# INLINE getIP4 #-}

putIP4 :: Putter IP4
putIP4 (IP4 addr) = putWord32be (unsafeToIP4 addr)
{-# INLINE putIP4 #-}

-- | Make an IP4 from four address bytes.
packIP4 :: Word8 -> Word8 -> Word8 -> Word8 -> IP4
packIP4 a b c d = IP4 $! fromIP4 $! set (byte 3) a
                                 $! set (byte 2) b
                                 $! set (byte 1) c
                                 $! set (byte 0) d 0
{-# INLINE packIP4 #-}

-- | Unpack an IP4 to four address bytes.
unpackIP4 :: IP4 -> (Word8,Word8,Word8,Word8)
unpackIP4 (IP4 (IP6 _ w64)) = ( view (byte 3) w64
                              , view (byte 2) w64
                              , view (byte 1) w64
                              , view (byte 0) w64
                              )
{-# INLINE unpackIP4 #-}

showIP4 :: IP4 -> ShowS
showIP4 ip4 =
  let (a,b,c,d) = unpackIP4 ip4
   in shows a . showChar '.' .
      shows b . showChar '.' .
      shows c . showChar '.' .
      shows d
{-# INLINE showIP4 #-}

readIP4 :: ReadS IP4
readIP4 str =
  do (a,'.':rest1) <- readDec str
     (b,'.':rest2) <- readDec rest1
     (c,'.':rest3) <- readDec rest2
     (d,rest4)     <- readDec rest3
     return (packIP4 a b c d, rest4)
{-# INLINE readIP4 #-}


newtype IP4Mask = IP4Mask IP6Mask
                  deriving (Show,Read,Eq)

broadcastAddress :: IP4Mask -> IP4
broadcastAddress  = setHostBits

readIP4Mask :: ReadS IP4Mask
readIP4Mask str =
  do (IP4 addr,'/':rest1) <- readIP4 str
     (bits,rest2)         <- readDec rest1
     return (IP4Mask (IP6Mask addr bits), rest2)

showIP4Mask :: IP4Mask -> ShowS
showIP4Mask m = showIP4 (maskAddr m) . showChar '/' . shows (maskBits m)


class IsMask mask addr | mask -> addr, addr -> mask where
  toIP6Mask     :: mask -> IP6Mask

  mkMask        :: addr -> Int -> mask

  netmask       :: Int  -> addr
  hostmask      :: Int  -> addr
  clearHostBits :: mask -> addr
  setHostBits   :: mask -> addr

  maskRange     :: mask -> (addr,addr)
  maskRange  mask = (clearHostBits mask, setHostBits mask)

  maskAddr      :: mask -> addr
  maskBits      :: mask -> Int


instance IsMask IP6Mask IP6 where
  toIP6Mask = id

  mkMask = IP6Mask

  netmask i =
    case hostmask i of
      IP6 a b -> IP6 (complement a) (complement b)

  hostmask i = IP6 a b
    where
    i' = 128 - i
    a  = bit (max 0 (i' - 64)) - 1
    b  = bit (max 0 i')        - 1

  clearHostBits (IP6Mask (IP6 a b) bits) =
    case netmask bits of
      IP6 ma mb -> IP6 (a .&. ma) (b .&. mb)

  setHostBits (IP6Mask (IP6 a b) bits) =
    case hostmask bits of
      IP6 ma mb -> IP6 (a .|. ma) (b .|. mb)

  maskAddr (IP6Mask addr _) = addr

  maskBits (IP6Mask _ bits) = bits

  {-# INLINE toIP6Mask #-}
  {-# INLINE mkMask #-}
  {-# INLINE netmask #-}
  {-# INLINE hostmask #-}
  {-# INLINE clearHostBits #-}
  {-# INLINE setHostBits #-}
  {-# INLINE maskAddr #-}
  {-# INLINE maskBits #-}


instance IsMask IP4Mask IP4 where
  toIP6Mask (IP4Mask mask) = mask

  mkMask (IP4 addr) bits = IP4Mask (IP6Mask addr bits)

  netmask i = IP4 (fromIP4 (complement (bit i' - 1)))
    where
    i' = 32 - i

  hostmask i = IP4 (fromIP4 (bit i' - 1))
    where
    i' = 32 - i

  clearHostBits (IP4Mask (IP6Mask (IP6 _ b) bits)) =
    case netmask bits of
      IP4 (IP6 _ mb) -> IP4 (IP6 0x0 (b .&. mb))

  setHostBits (IP4Mask (IP6Mask (IP6 _ b) bits)) =
    case netmask bits of
      IP4 (IP6 _ mb) -> IP4 (IP6 0x0 (b .|. mb))

  maskAddr (IP4Mask (IP6Mask addr _)) = IP4 addr
  maskBits (IP4Mask (IP6Mask _ bits)) = bits

  {-# INLINE toIP6Mask #-}
  {-# INLINE mkMask #-}
  {-# INLINE netmask #-}
  {-# INLINE hostmask #-}
  {-# INLINE clearHostBits #-}
  {-# INLINE setHostBits #-}
  {-# INLINE maskAddr #-}
  {-# INLINE maskBits #-}
