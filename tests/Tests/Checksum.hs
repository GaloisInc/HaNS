module Tests.Checksum where

import Hans.Checksum (computeChecksum)

import Data.Serialize (runPut,putWord32be,putWord16be)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck


checksumTests :: TestTree
checksumTests  = testGroup "Checksum"
  [ testProperty "Word16"         $
    forAll arbitraryBoundedRandom $ \ w16 ->
      computeChecksum (runPut (putWord16be w16)) === computeChecksum w16

  , testProperty "Word32"         $
    forAll arbitraryBoundedRandom $ \ w32 ->
      computeChecksum (runPut (putWord32be w32)) === computeChecksum w32
  ]
