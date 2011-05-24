
module Hans.Address.Mac (
    Mac(..)
  , showsMac
  , macMask
  ) where

import Hans.Address
import Hans.Utils (showPaddedHex)

import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getWord16be,getWord32be)
import Data.Serialize.Put (putByteString)
import Data.Bits (Bits(shiftR,testBit,complement))
import Data.List (intersperse)
import Data.Word (Word8)
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
  deriving ( Eq, Ord )


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

instance Serialize Mac where
  get = do
    n <- getWord32be
    m <- getWord16be
    let f x d = fromIntegral (x `shiftR` d)
    return $! Mac (f n 24) (f n 16) (f n 8) (fromIntegral n)
                  (f m 8)  (fromIntegral m)

  put (Mac a b c d e f) = putByteString (S.pack [a,b,c,d,e,f])
