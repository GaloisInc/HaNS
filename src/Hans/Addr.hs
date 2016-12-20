{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Hans.Addr (
  -- * Addresses
  IsAddr(..),

  -- ** IPv6
  IP6(), isIP4, toIP4,
  getIP6, putIP6,
  packIP6, unpackIP6,
  showIP6, readIP6,

  -- *** Unicast
  interfaceIdentifier, routingPrefixSubnetId,

  -- *** Link-local
  linkLocalPrefix,

  -- *** Multicast
  multicast, multicastR, multicastP, multicastT,

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
  IP6Mask(..),
  readIP6Mask, showIP6Mask,

  -- ** IPv4
  IP4Mask(), pattern IP4Mask,
  broadcastAddress,
  readIP4Mask, showIP4Mask,

  ) where

import Hans.Checksum (Checksum(..))
import Hans.Lens as Lens (Getting,Lens',view,byte,set,bit,to)

import Control.Monad(void,unless)
import Data.Bits as Bits ((.&.),(.|.),shiftR,shiftL,bit,complement)
import Data.Char (isHexDigit,digitToInt)
import Data.List (intercalate)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize(..),Putter,Get,getWord32be,putWord32be
                      ,putWord64be,getWord64be)
import Data.Typeable (Typeable)
import Data.Word (Word8,Word16,Word32,Word64)
import GHC.Generics (Generic)
import Numeric (readDec,showHex)
import Text.ParserCombinators.ReadP(ReadP,choice,option,char,satisfy,readP_to_S)


-- IPv6 Addresses --------------------------------------------------------------

-- | Generic IP4/IP6 address type.
data IP6 = IP6 {-# UNPACK #-} !Word64
               {-# UNPACK #-} !Word64
           deriving (Eq,Ord,Show,Read,Generic,Typeable)

front :: Lens' IP6 Word64
front f = \ (IP6 a b) -> fmap (\ a' -> IP6 a' b) (f a)

back :: Lens' IP6 Word64
back f = \ (IP6 a b) -> fmap (\ b' -> IP6 a b') (f b)

instance Hashable IP6

instance Checksum IP6 where
  extendChecksum (IP6 a b) = \pc -> extendChecksum a (extendChecksum b pc)
  {-# INLINE extendChecksum #-}

packIP6 :: Word64 -> Word64 -> IP6
packIP6 a b = IP6 a b
{-# INLINE packIP6 #-}

unpackIP6 :: IP6 -> (Word64, Word64)
unpackIP6 (IP6 a b) = (a,b)
{-# INLINE unpackIP6 #-}

getIP6 :: Get IP6
getIP6 = IP6 <$> getWord64be <*> getWord64be
{-# INLINE getIP6 #-}

putIP6 :: Putter IP6
putIP6 (IP6 a b) =
  do putWord64be a
     putWord64be b
{-# INLINE putIP6 #-}

readIP6 :: ReadS IP6
readIP6 = readP_to_S parseIP6

parseIP6 :: ReadP IP6
parseIP6 =
  do listform <- choice [startsWithDouble, startsWithChunk]
     unless (length listform == 8) $ fail "bad length"
     let ([a,b,c,d],[e,f,g,h]) = splitAt 4 listform
     return (IP6 (unsplit64 a b c d) (unsplit64 e f g h))
 where
  startsWithDouble :: ReadP [Word16]
  startsWithDouble =
    do doubleColon
       option (replicate 8 0) $
         do x      <- readIP6Chunk
            result <- normalOperation [x]
            let numZeros = 8 - length result
            return (replicate numZeros 0 ++ result)
  startsWithChunk :: ReadP [Word16]
  startsWithChunk =
    do chunk1 <- readIP6Chunk
       doublePossible [chunk1]
  doublePossible :: [Word16] -> ReadP [Word16]
  doublePossible acc =
    option (reverse acc) $
      choice [hitDouble acc, hitSingle acc]
  hitDouble :: [Word16] -> ReadP [Word16]
  hitDouble acc =
    do doubleColon
       option (let numZeros = 8 - length acc
                   zeros    = replicate numZeros 0
               in reverse acc ++ zeros) $
         do x    <- readIP6Chunk
            rest <- normalOperation [x]
            let numZeros = 8 - (length acc + length rest)
                zeros    = replicate numZeros 0
            return (reverse acc ++ zeros ++ rest)
  hitSingle :: [Word16] -> ReadP [Word16]
  hitSingle acc =
    do void (char ':')
       chunk <- readIP6Chunk
       doublePossible (chunk : acc)
  normalOperation :: [Word16] -> ReadP [Word16]
  normalOperation acc =
    option (reverse acc) $
      do void (char ':')
         chunk <- readIP6Chunk
         normalOperation (chunk : acc)

doubleColon :: ReadP ()
doubleColon =
  do void (char ':')
     void (char ':')

readIP6Chunk :: ReadP Word16
readIP6Chunk =
  do a <- hexDigit
     option a $
       do b <- hexDigit
          let res2 = (a `shiftL` 4) + b
          option res2 $
            do c <- hexDigit
               let res3 = (res2 `shiftL` 4) + c
               option res3 $
                 do d <- hexDigit
                    return ((res3 `shiftL` 4) + d)

hexDigit :: Integral a => ReadP a
hexDigit =
  do a <- satisfy isHexDigit
     return (fromIntegral (digitToInt a))

showIP6 :: IP6 -> ShowS
showIP6 (IP6 x y) = 
  let base     = split64 x ++ split64 y
      zeroMap  = numZeros base
      maxZeros = maximum (map snd zeroMap)
      result   = go maxZeros zeroMap
  in \ rest -> result ++ rest
 where
  go :: Int -> [(Word16, Int)] -> String
  go _ [] = ""
  go maxZeros ((0,count):rest) | count == maxZeros =
    let nonZeros = drop (count - 1) rest
        digits   = map fst nonZeros
        restStr  = intercalate ":" (map showHex' digits)
    in "::" ++ restStr
  go maxZeros ((v,_):rest) =
    let res = go maxZeros rest
    in case res of
         (':':_)             -> showHex v res
         _       | null rest -> showHex v ""
         _                   -> showHex v ":" ++ res
  --
  numZeros :: [Word16] -> [(Word16, Int)]
  numZeros [] = []
  numZeros (0:rest) =
    let count = 1 + length (takeWhile (== 0) rest)
    in (0, count) : numZeros rest
  numZeros (v:rest) =
    (v,0) : numZeros rest

unsplit64 :: Word16 -> Word16 -> Word16 -> Word16 -> Word64
unsplit64 a b c d =
  ((fromIntegral a `shiftL` 48) .|.
   (fromIntegral b `shiftL` 32) .|.
   (fromIntegral c `shiftL` 16) .|.
   (fromIntegral d))

split64 :: Word64 -> [Word16]
split64 x = [ fromIntegral (x `shiftR` 48)
            , fromIntegral (x `shiftR` 32)
            , fromIntegral (x `shiftR` 16)
            , fromIntegral x ]

showHex' :: (Show a, Integral a) => a -> String
showHex' x = showHex x ""

data IP6Mask = IP6Mask {-# UNPACK #-} !IP6
                       {-# UNPACK #-} !Int
               deriving (Ord,Show,Read,Generic,Typeable)

instance Eq IP6Mask where
  IP6Mask a b == IP6Mask c d = b == d && a == c

instance Hashable IP6Mask

readIP6Mask :: ReadS IP6Mask
readIP6Mask str =
  do (addr,'/':rest1) <- readIP6 str
     (bits,rest2)     <- readDec rest1
     return (IP6Mask addr bits, rest2)

showIP6Mask :: IP6Mask -> ShowS
showIP6Mask m = showIP6 (maskAddr m) . showChar '/' . shows (maskBits m)

interfaceIdentifier :: Lens' IP6 Word64
interfaceIdentifier  = back
{-# INLINE interfaceIdentifier #-}

routingPrefixSubnetId :: Int -> Lens' IP6 (Word64,Word64)
routingPrefixSubnetId len f =
  let off  = 64 - min 64 (max 48 len)
      mask = Bits.bit off - 1
   in \ (IP6 a b) ->
        fmap (\ (x,y) -> IP6 (x `shiftL` off .|. y .&. mask) b)
             (f (a `shiftR` off, a .&. mask))
{-# INLINE routingPrefixSubnetId #-}

linkLocalPrefix :: Lens' IP6 Word16
linkLocalPrefix f = \ (IP6 a b) ->
  fmap (\w -> IP6 ((fromIntegral w `shiftL` 54) .|. (a .&. mask)) b)
       (f (fromIntegral (a `shiftR` 54)))
  where
  mask = Bits.bit 54 - 1
{-# INLINE linkLocalPrefix #-}

-- Should this be a Lens'?
multicast :: Getting r IP6 Bool
multicast  = front . to l
  where
  mask = 0xff `shiftL` 56
  l a  = a .&. mask == mask
{-# INLINE multicast #-}

multicastR :: Lens' IP6 Bool
multicastR  = front . Lens.bit 54
{-# INLINE multicastR #-}

multicastP :: Lens' IP6 Bool
multicastP  = front . Lens.bit 53
{-# INLINE multicastP #-}

multicastT :: Lens' IP6 Bool
multicastT  = front . Lens.bit 52
{-# INLINE multicastT #-}

-- IPv4 Addresses --------------------------------------------------------------

isIP4 :: IP6 -> Bool
isIP4 (IP6 0x0 w64) = w64 `shiftR` 32 == 0xffff
isIP4 _             = False

toIP4 :: IP6 -> Maybe IP4
toIP4 addr | isIP4 addr = Just (IP4 addr)
toIP4 _                 = Nothing

pattern Addr4 addr <- (toIP4 -> Just addr)
  where
  Addr4 (IP4 addr) = addr


newtype IP4 = IP4 IP6
              deriving (Eq,Ord,Show,Read,Generic,Typeable,Checksum,Hashable)

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


newtype IP4Mask = IP4Mask_ IP6Mask
                  deriving (Show,Read,Eq)

pattern IP4Mask addr bits <- IP4Mask_ (IP6Mask (IP4 -> addr) bits)
  where
  IP4Mask (IP4 addr) bits = IP4Mask_ (IP6Mask addr bits)

broadcastAddress :: IP4Mask -> IP4
broadcastAddress  = setHostBits

readIP4Mask :: ReadS IP4Mask
readIP4Mask str =
  do (IP4 addr,'/':rest1) <- readIP4 str
     (bits,rest2)         <- readDec rest1
     return (IP4Mask_ (IP6Mask addr bits), rest2)

showIP4Mask :: IP4Mask -> ShowS
showIP4Mask m = showIP4 (maskAddr m) . showChar '/' . shows (maskBits m)


class IsMask mask addr | mask -> addr, addr -> mask where
  toIP6Mask     :: mask -> IP6Mask

  netmask       :: Int  -> addr
  hostmask      :: Int  -> addr
  clearHostBits :: mask -> addr
  setHostBits   :: mask -> addr

  maskRange     :: mask -> (addr,addr)
  maskRange m = (clearHostBits m, setHostBits m)

  maskAddr      :: mask -> addr
  maskBits      :: mask -> Int

  isMember      :: mask -> addr -> Bool


instance IsMask IP6Mask IP6 where
  toIP6Mask = id

  netmask i =
    case hostmask i of
      IP6 a b -> IP6 (complement a) (complement b)

  hostmask i = IP6 a b
    where
    i' = 128 - i
    a  = Bits.bit (max 0 (i' - 64)) - 1
    b  = Bits.bit (max 0 i')        - 1

  clearHostBits (IP6Mask (IP6 a b) bits) =
    case netmask bits of
      IP6 ma mb -> IP6 (a .&. ma) (b .&. mb)

  setHostBits (IP6Mask (IP6 a b) bits) =
    case hostmask bits of
      IP6 ma mb -> IP6 (a .|. ma) (b .|. mb)

  maskAddr (IP6Mask addr _) = addr

  maskBits (IP6Mask _ bits) = bits

  isMember (IP6Mask (IP6 a b) bits) =
    let IP6 ma mb = netmask bits
        pa        = a .&. ma
        pb        = b .&. mb
     in \ (IP6 x y) -> (x .&. ma) == pa && (y .&. mb) == pb

  {-# INLINE toIP6Mask #-}
  {-# INLINE netmask #-}
  {-# INLINE hostmask #-}
  {-# INLINE clearHostBits #-}
  {-# INLINE setHostBits #-}
  {-# INLINE maskAddr #-}
  {-# INLINE maskBits #-}
  {-# INLINE isMember #-}


instance IsMask IP4Mask IP4 where
  toIP6Mask (IP4Mask_ mask) = mask

  netmask i = IP4 (IP6 0x0 (0xffff00000000 .|. complement (Bits.bit i' - 1)))
    where
    i' = 32 - i

  hostmask i = IP4 (IP6 0x0 (0xffff00000000 .&. (Bits.bit i' - 1)))
    where
    i' = 32 - i

  clearHostBits (IP4Mask_ (IP6Mask (IP6 _ b) bits)) =
    case netmask bits of
      IP4 (IP6 _ mb) -> IP4 (IP6 0x0 (b .&. mb))

  setHostBits (IP4Mask_ (IP6Mask (IP6 _ b) bits)) =
    case netmask bits of
      IP4 (IP6 _ mb) -> IP4 (IP6 0x0 (b .|. mb))

  maskAddr (IP4Mask_ (IP6Mask addr _)) = IP4 addr
  maskBits (IP4Mask_ (IP6Mask _ bits)) = bits

  isMember (IP4Mask_ (IP6Mask (IP6 _ b) bits)) =
    let IP4 (IP6 _ mb) = netmask bits
        pb             = b .&. mb
     in \ (IP4 (IP6 _ y)) -> (y .&. mb) == pb

  {-# INLINE toIP6Mask #-}
  {-# INLINE netmask #-}
  {-# INLINE hostmask #-}
  {-# INLINE clearHostBits #-}
  {-# INLINE setHostBits #-}
  {-# INLINE maskAddr #-}
  {-# INLINE maskBits #-}
  {-# INLINE isMember #-}


class (Typeable addr, Eq addr) => IsAddr addr where
  toIP6        :: addr -> IP6
  fromIP6      :: IP6  -> Maybe addr
  isWildcard   :: addr -> Bool
  wildcardAddr :: addr -> addr

  -- | Check to see if this is a broadcast/multicast address.
  isBroadcast  :: addr -> Bool

instance IsAddr IP6 where
  toIP6   = id
  fromIP6 = Just

  isWildcard (IP6 0x0 0x0) = True
  isWildcard _             = False

  wildcardAddr _ = IP6 0x0 0x0

  isBroadcast = view multicast

  {-# INLINE toIP6 #-}
  {-# INLINE fromIP6 #-}
  {-# INLINE isWildcard #-}
  {-# INLINE wildcardAddr #-}
  {-# INLINE isBroadcast #-}

instance IsAddr IP4 where
  toIP6 (IP4 addr) = addr
  fromIP6          = toIP4

  isWildcard (IP4 (IP6 _ 0xffff00000000)) = True
  isWildcard _                            = False

  wildcardAddr _ = IP4 (IP6 0x0 0xffff00000000)

  isBroadcast addr = addr == BroadcastIP4

  {-# INLINE toIP6 #-}
  {-# INLINE fromIP6 #-}
  {-# INLINE isWildcard #-}
  {-# INLINE wildcardAddr #-}
  {-# INLINE isBroadcast #-}
