module Udp.Packet where

import Hans.Message.Udp
    (UdpHeader(..),UdpPort(..),parseUdpHeader,renderUdpHeader)

import Control.Applicative ((<$>),(<*>))
import Data.Serialize (runGet,runPut)
import System.Random (Random)
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen,forAll,arbitraryBoundedRandom,arbitrary,choose)
import Test.QuickCheck.Property (Result(..),failed,succeeded)


-- Utilities -------------------------------------------------------------------

arbitraryUdpPacketSize :: (Random a, Num a) => Gen a
arbitraryUdpPacketSize  = choose (0,65535 - 8)

arbitraryUdpPort :: Gen UdpPort
arbitraryUdpPort  = UdpPort <$> arbitraryBoundedRandom

arbitraryUdpHeader :: Gen UdpHeader
arbitraryUdpHeader  = UdpHeader
                  <$> arbitraryUdpPort
                  <*> arbitraryUdpPort
                  <*> arbitraryBoundedRandom -- not necessarily right


-- Properties ------------------------------------------------------------------

udpPacketTests :: Test
udpPacketTests  = testGroup "packet parsing"
  [ testProperty "prop_headerRoundTrip" prop_headerRoundTrip
  ]

prop_headerRoundTrip = forAll packet $ \ (len,hdr) ->
  case runGet parseUdpHeader (runPut (renderUdpHeader hdr len)) of
    Right (hdr',len') | hdr == hdr' && len == len' -> succeeded
                      | otherwise                  -> failed
    Left err                                       -> failed { reason = err }
  where
  packet = (,) <$> arbitraryUdpPacketSize <*> arbitraryUdpHeader
