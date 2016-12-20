{-# LANGUAGE PatternSynonyms #-}
module Tests.Address where

import Hans.Addr
import Test.QuickCheck(Gen,arbitrary,choose)
import Test.Tasty (testGroup,TestTree)
import Tests.Utils (encodeDecodeIdentity,showReadIdentity)
import Test.Tasty.QuickCheck (testProperty)

arbitraryIP4 :: Gen IP4
arbitraryIP4 = packIP4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

arbitraryIP4Mask :: Gen IP4Mask
arbitraryIP4Mask = IP4Mask <$> arbitraryIP4 <*> choose (0,32)

arbitraryIP6 :: Gen IP6
arbitraryIP6 = packIP6 <$> arbitrary <*> arbitrary

arbitraryIP6Mask :: Gen IP6Mask
arbitraryIP6Mask = IP6Mask <$> arbitraryIP6 <*> choose (0,128)

addressTests :: TestTree
addressTests = testGroup "Addresses"
  [ testProperty "IP4 address binary encode/decode" $
    encodeDecodeIdentity putIP4 getIP4 arbitraryIP4
  , testProperty "IP4 address ASCII encode/decode" $
    showReadIdentity showIP4 readIP4 arbitraryIP4
  , testProperty "IP4 address mask ASCII encode/decode" $
    showReadIdentity showIP4Mask readIP4Mask arbitraryIP4Mask
  , testProperty "IP6 address binary encode/decode" $
    encodeDecodeIdentity putIP6 getIP6 arbitraryIP6
  , testProperty "IP6 address ASCII encode/decode" $
    showReadIdentity showIP6 readIP6 arbitraryIP6
  , testProperty "IP6 address mask ASCII encode/decode" $
    showReadIdentity showIP6Mask readIP6Mask arbitraryIP6Mask
  ]
