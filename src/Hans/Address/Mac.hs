{-# LANGUAGE DeriveGeneric #-}

module Hans.Address.Mac (
    Mac(..)
  , parseMac
  , renderMac
  , broadcastMac
  , showsMac
  , macMask
  ) where

import Hans.Address
import Hans.Utils (showPaddedHex)

import Control.Applicative ((<*>),(<$>))
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get,getWord8)
import Data.Serialize.Put (Putter,putByteString)
import Data.Bits (Bits(testBit,complement), shift)
import Data.List (intersperse)
import Data.Word (Word8)
import GHC.Generics ( Generic )
import Numeric (readHex)
import qualified Data.ByteString as S


-- | Mac addresses.
data Mac = Mac
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  deriving ( Eq, Ord, Generic )


-- | Show a Mac address.
showsMac :: Mac -> ShowS
showsMac (Mac a b c d e f) = foldl1 (.)
                           $ intersperse (showChar ':')
                           $ map showPaddedHex [a,b,c,d,e,f]

-- | Generates a mask tailored to the given MAC address.
macMask :: Mac -> Mac
macMask (Mac a b c d e f) =
  Mac (complement a)
      (complement b)
      (complement c)
      (complement d)
      (complement e)
      (complement f)

-- | The broadcast MAC address.
broadcastMac :: Mac
broadcastMac  = Mac 0xff 0xff 0xff 0xff 0xff 0xff

-- | Multicast MAC addresses will have an odd first octet
multicastMac :: Mac -> Bool
multicastMac (Mac a _ _ _ _ _) = (a `shift` 7) /= 0

instance Show Mac where
  showsPrec _ = showsMac

instance Read Mac where
  readsPrec _ = loop 6 []
    where
    loop :: Int -> [Word8] -> String -> [(Mac,String)]
    loop 0 [f,e,d,c,b,a] str = [(Mac a b c d e f,str)]
    loop 0 _             _   = []
    loop n acc           str = case readHex str of
      [(a,':':rest)] -> loop (n-1) (a:acc) rest
      [(a,    rest)] -> loop 0     (a:acc) rest
      _              -> []


instance Address Mac where
  addrSize _ = 6

  toBits (Mac a b c d e f) = concatMap k [a,b,c,d,e,f]
    where k i = map (testBit i) [0 .. 7]

parseMac :: Get Mac
parseMac  =  Mac
         <$> getWord8
         <*> getWord8
         <*> getWord8
         <*> getWord8
         <*> getWord8
         <*> getWord8

renderMac :: Putter Mac
renderMac (Mac a b c d e f) = putByteString (S.pack [a,b,c,d,e,f])

instance Serialize Mac where
  get = parseMac
  put = renderMac
