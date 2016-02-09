{-# LANGUAGE RecordWildCards #-}

module Tests.Ethernet where

import Tests.Utils (encodeDecodeIdentity,showReadIdentity)

import Hans.Ethernet

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)


arbitraryMac :: Gen Mac
arbitraryMac  =
  do a <- arbitraryBoundedIntegral
     b <- arbitraryBoundedIntegral
     c <- arbitraryBoundedIntegral
     d <- arbitraryBoundedIntegral
     e <- arbitraryBoundedIntegral
     f <- arbitraryBoundedIntegral
     return (Mac a b c d e f)

arbitrayEthernetHeader :: Gen EthernetHeader
arbitrayEthernetHeader  =
  do eSource <- arbitraryMac
     eDest   <- arbitraryMac
     eType   <- arbitraryBoundedRandom
     return EthernetHeader { .. }


-- Ethernet Properties ---------------------------------------------------------

ethernetTests :: TestTree
ethernetTests  = testGroup "Ethernet"
  [ testProperty "Mac Address encode/decode" $
    encodeDecodeIdentity putMac getMac arbitraryMac

  , testProperty "Header encode/decode" $
    encodeDecodeIdentity putEthernetHeader getEthernetHeader arbitrayEthernetHeader

  , testProperty "Mac Address read/show" $
    showReadIdentity showMac readMac arbitraryMac
  ]
