module IP4.Packet where

import IP4.Addr
import Utils

import Hans.Message.Ip4

import Control.Applicative ((<$>))
import Data.Word (Word16)
import System.Random ()
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen,arbitrary,arbitraryBoundedRandom,choose,forAll)


-- Packet Generation -----------------------------------------------------------

arbitraryIP4Header :: Gen IP4Header
arbitraryIP4Header  = do
  tos   <- arbitraryBoundedRandom
  ident <- arbitraryIdent
  df    <- arbitrary
  mf    <- arbitrary
  off   <- arbitraryFragmentOffset
  ttl   <- arbitraryBoundedRandom
  prot  <- arbitraryIP4Protocol
  src   <- arbitraryIP4
  dst   <- arbitraryIP4
  return emptyIP4Header
    { ip4TypeOfService  = tos
    , ip4DontFragment   = df
    , ip4MoreFragments  = mf
    , ip4FragmentOffset = off
    , ip4TimeToLive     = ttl
    , ip4Protocol       = prot
    , ip4SourceAddr     = src
    , ip4DestAddr       = dst
    }

arbitraryIdent :: Gen Ident
arbitraryIdent  = Ident <$> arbitraryBoundedRandom

arbitraryFragmentOffset :: Gen Word16
arbitraryFragmentOffset  = (8*) <$> choose (0,0x1fff)

arbitraryIP4Protocol :: Gen IP4Protocol
arbitraryIP4Protocol  = IP4Protocol <$> arbitraryBoundedRandom

arbitraryIP4PacketLen :: Gen Int
arbitraryIP4PacketLen  = choose (0,65535 - 20)


-- Packet Tests ----------------------------------------------------------------

ip4PacketTests :: Test
ip4PacketTests  = testGroup "packet parsing"
  [ testProperty "prop_headerRoundTrip" prop_headerRoundTrip
  ]

prop_headerRoundTrip = forAll arbitraryIP4PacketLen $ \ pktLen ->
  let parse = do
        (hdr,_,_) <- getIP4Packet
        return hdr
      render hdr = putIP4Header hdr pktLen
   in roundTrip arbitraryIP4Header parse render

