module Tests.IP4.Packet where

import Hans.IP4.Packet

import Test.QuickCheck

import qualified Data.ByteString.Lazy as L


arbitraryIP4 :: Gen IP4
arbitraryIP4  =
  do a <- arbitraryBoundedRandom
     b <- arbitraryBoundedRandom
     c <- arbitraryBoundedRandom
     d <- arbitraryBoundedRandom
     return $! packIP4 a b c d


arbitraryProtocol :: Gen IP4Protocol
arbitraryProtocol  = arbitraryBoundedRandom


arbitraryIdent :: Gen IP4Ident
arbitraryIdent  = arbitraryBoundedRandom


arbitraryPayload :: Int -> Int -> Gen L.ByteString
arbitraryPayload lo hi =
  do len   <- choose (lo,hi)
     bytes <- vectorOf len arbitraryBoundedRandom
     return (L.pack bytes)
