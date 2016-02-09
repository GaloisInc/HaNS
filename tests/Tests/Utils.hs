module Tests.Utils where

import Test.QuickCheck

import Data.Serialize (Putter,Get,runGet,runPut)


encodeDecodeIdentity :: (Eq a, Show a) => Putter a -> Get a -> Gen a -> Property
encodeDecodeIdentity put get gen =
  forAll gen $ \ a ->
    case runGet get (runPut (put a)) of
      Right a'  -> a === a'
      Left str  -> property False

showReadIdentity :: (Eq a, Show a) => (a -> ShowS) -> ReadS a -> Gen a -> Property
showReadIdentity sw rd gen =
  forAll gen $ \ a ->
    case rd (sw a "") of
      [(a',_)] -> a === a'
      _        -> property False
