module Utils where

import Data.Serialize (runGet,runPut,Putter,Get)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen,forAll)
import Test.QuickCheck.Property (Property,Result(..),succeeded,failed)


-- | Round trip something through cereal, making sure that rendered version
-- parses to the same initial value.
roundTrip :: (Show a, Eq a) => Gen a -> Get a -> Putter a -> Property
roundTrip gen decode encode = forAll gen $ \ a ->
  case runGet decode (runPut (encode a)) of
    Right a' | a == a'   -> succeeded
             | otherwise -> failed
    Left err             -> failed { reason = err }
