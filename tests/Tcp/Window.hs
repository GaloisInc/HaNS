module Tcp.Window where

import Tcp.Packet

import Hans.Layer.Tcp.Window
    (emptyLocalWindow,addInSegment,stepWindow,InSegment(..))
import Hans.Message.Tcp (TcpHeader(..))

import Control.Arrow ((&&&))
import Test.Framework (Test,testGroup)
import Test.QuickCheck (forAll)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.ByteString as S
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


tcpWindowTests :: Test
tcpWindowTests  = testGroup "tcp window"
  [ testProperty "localOrdered" prop_localOrdered
  , testProperty "localRandom"  prop_localRandom
  ]


-- Local Window ----------------------------------------------------------------

fromInSegment :: InSegment -> (TcpHeader,S.ByteString)
fromInSegment  = isHeader &&& isBody

-- | Check that if a stream of packets goes into the @LocalWindow@ in order,
-- that they will come out in the same order.
prop_localOrdered = forAll packetStream $ \ segs ->
  let (hdr,_)         = Seq.index segs 0
      win             = emptyLocalWindow (tcpSeqNum hdr)
      step w (h,body) = addInSegment h body w
      win'            = F.foldl step win segs
      (segs',_)       = stepWindow win'
   in segs == fmap fromInSegment segs'

-- | Check that if a stream of initially ordered packets goes into the
-- @LocalWindow@ in a random order, that they come out ordered again.  In this
-- case, we just reverse the stream, as that should be the degenrate case for
-- incoming packets.
prop_localRandom = forAll packetStream $ \ segs ->
  let (hdr,_)         = Seq.index segs 0
      win             = emptyLocalWindow (tcpSeqNum hdr)
      step (h,body) w = addInSegment h body w
      win'            = F.foldr step win segs
      (segs',_)       = stepWindow win'
   in segs == fmap fromInSegment segs'
