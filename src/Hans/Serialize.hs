module Hans.Serialize where

import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Lazy as L
import           Data.Serialize (Put,execPut)


-- | A specialized version of 'runPut' that allows the initial and successive
-- buffer sizes for the 'Builder' to be defined. For example, if you're
-- rendering an IP4 packet, you might give the first parameter as 20 (the size
-- of the fixed header), and the second as 40 (the maximum additional length for
-- options).
runPutPacket :: Int -> Int -> L.ByteString -> Put -> L.ByteString
runPutPacket isz ssz tl m =
  B.toLazyByteStringWith (B.untrimmedStrategy isz ssz) tl (execPut m)
