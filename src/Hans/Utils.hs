module Hans.Utils where

import Control.Monad (MonadPlus(mzero))
import Data.ByteString (ByteString)
import Numeric (showHex)

type DeviceName = String

type Packet = ByteString

type MkPseudoHeader = Int -> Packet

type Endo a = a -> a

-- | Discard the result of a monadic computation.
void :: Monad m => m a -> m ()
void m = m >> return ()

-- | Show a single hex number, padded with a leading 0.
showPaddedHex :: (Integral a) => a -> ShowS
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
