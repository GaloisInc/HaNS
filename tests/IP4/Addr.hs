module IP4.Addr where

import Hans.Address.IP4

import Control.Applicative ((<$>),(<*>))
import System.Random ()
import Test.QuickCheck (Gen,arbitraryBoundedRandom)


arbitraryIP4 :: Gen IP4
arbitraryIP4  = IP4
            <$> arbitraryBoundedRandom
            <*> arbitraryBoundedRandom
            <*> arbitraryBoundedRandom
            <*> arbitraryBoundedRandom
