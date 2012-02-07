module Hans.Utils where

import Control.Monad (MonadPlus(mzero))
import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S

type DeviceName = String

-- | Pseudo headers are constructed strictly.
type MkPseudoHeader = Int -> S.ByteString

type Endo a = a -> a

-- | Discard the result of a monadic computation.
void :: Monad m => m a -> m ()
void m = m >> return ()

-- | Show a single hex number, padded with a leading 0.
showPaddedHex :: (Integral a, Show a) => a -> ShowS
showPaddedHex x
  | x < 0x10  = showChar '0' . base
  | otherwise = base
  where base = showHex x

-- | Lift a maybe into MonadPlus
just :: MonadPlus m => Maybe a -> m a
just  = maybe mzero return

-- | Make a singleton list.
singleton :: a -> [a]
singleton x = [x]

-- | Make a lazy bytestring from a strict one.
chunk :: S.ByteString -> L.ByteString
chunk bs = L.fromChunks [bs]

strict :: L.ByteString -> S.ByteString
strict  = S.concat . L.toChunks
