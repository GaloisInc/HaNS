module Tests.Network where

import Hans.Network.Types

import Test.QuickCheck (Gen,arbitraryBoundedRandom)


arbitraryProtocol :: Gen NetworkProtocol
arbitraryProtocol  = arbitraryBoundedRandom
