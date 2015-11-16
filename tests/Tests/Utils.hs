module Tests.Utils where

import Test.QuickCheck

import Data.Serialize (Putter,Get,runGet,runPut)


encodeDecodeIdentity :: (Eq a, Show a) => Putter a -> Get a -> Gen a -> Property
encodeDecodeIdentity put get gen =
  forAll gen $ \ a ->
    case runGet get (runPut (put a)) of
      Right a'  -> label "Serialization" (a === a')
      Left str  -> label "Decode error" False
