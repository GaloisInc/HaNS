{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tcp.Packet where

import Hans.Message.Tcp
    (TcpHeader(..),emptyTcpHeader,TcpPort(..),TcpOption(..),getTcpHeader
    ,putTcpHeader,getTcpOption,putTcpOption,tcpOptionsLength)

import Control.Applicative (pure,(<*>),(<$>))
import Control.Monad (replicateM)
import Data.Serialize (runGet,runPut)
import Data.Word (Word8)
import System.Random ()
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
    (Gen,choose,arbitrarySizedIntegral,sized,arbitrary,listOf,oneof,forAll
    ,suchThat)
import Test.QuickCheck.Property (Result(..),failed,succeeded)
import qualified Data.ByteString as S
import qualified Data.Sequence as Seq


-- Utilities -------------------------------------------------------------------

-- | Generate a strict bytestring of nonzero length, that falls within the MSS
-- off the HaNS tcp layer.
arbitraryPayload :: Gen S.ByteString
arbitraryPayload  = do
  len   <- choose (1,10)
  S.pack `fmap` replicateM len arbitrarySizedIntegral

-- | Generate the starting point of a data packet stream.
arbitraryDataPacket :: Gen (TcpHeader,S.ByteString)
arbitraryDataPacket  = do
  body <- arbitraryPayload
  sn   <- arbitrarySizedIntegral
  an   <- arbitrarySizedIntegral
  let hdr = emptyTcpHeader
        { tcpSeqNum = sn
        , tcpAckNum = an
        , tcpAck    = True
        }
  return (hdr,body)

-- | Increment the sequence number, and generate some new data.
nextDataPacket :: (TcpHeader,S.ByteString) -> Gen (TcpHeader,S.ByteString)
nextDataPacket (hdr,body) = do
  body' <- arbitraryPayload
  let hdr' = hdr { tcpSeqNum = tcpSeqNum hdr + fromIntegral (S.length body) }
  return (hdr',body')

-- | Generate a sequence of headers and packet payloads.  This will yield a
-- non-empty stream whose length depends on the size parameter.
packetStream :: Gen (Seq.Seq (TcpHeader,S.ByteString))
packetStream  = do
  pkt@(hdr,body) <- arbitraryDataPacket
  sized (loop Seq.empty pkt)
  where
  loop segs pkt len
    | len == 0  = return (segs Seq.|> pkt)
    | otherwise = do
      pkt' <- nextDataPacket pkt
      loop (segs Seq.|> pkt) pkt' (len - 1)


arbitraryTcpPort :: Gen TcpPort
arbitraryTcpPort  = arbitrarySizedIntegral

-- | Generate a completely arbitrary header.  This may be a semantically
-- incorrect header, and is mainly useful for testing the parser.
arbitraryTcpHeader :: Gen TcpHeader
arbitraryTcpHeader  = TcpHeader
                  <$> arbitraryTcpPort
                  <*> arbitraryTcpPort
                  <*> arbitrarySizedIntegral
                  <*> arbitrarySizedIntegral
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrarySizedIntegral
                  <*> pure 0 -- checksum
                  <*> pure 0 -- urgent pointer
                  <*> arbitraryTcpOptions

arbitraryTcpOptions :: Gen [TcpOption]
arbitraryTcpOptions  = listOf arbitraryTcpOption `suchThat` (not . tooBig)
  where
  tooBig opts = fst (tcpOptionsLength opts) > 10

arbitraryTcpOption :: Gen TcpOption
arbitraryTcpOption  = oneof
  [ pure OptNoOption
  , OptMaxSegmentSize <$> arbitrarySizedIntegral
  , OptWindowScaling  <$> arbitrarySizedIntegral
  , OptTimestamp      <$> arbitrarySizedIntegral
                      <*> arbitrarySizedIntegral
  , do code  <- unusedTcpOptionNumber
       len   <- choose (0, 20)
       bytes <- replicateM (fromIntegral len) arbitrarySizedIntegral
       return (OptUnknown code (len + 2) (S.pack bytes))
  ]

-- | Choose an unused tcp option number.
unusedTcpOptionNumber :: Gen Word8
unusedTcpOptionNumber  =
  arbitrarySizedIntegral `suchThat` (not . (`elem` avoid))
  where
  -- it would be nice if this could could be generated from TcpOptionTag
  avoid = [0, 1, 2, 3, 8]


-- Properties ------------------------------------------------------------------

tcpPacketTests :: Test
tcpPacketTests  = testGroup "packet parsing"
  [ testProperty "prop_headerRoundTrip" prop_headerRoundTrip
  , testProperty "prop_optionRoundTrip" prop_optionRoundTrip
  ]

prop_headerRoundTrip = forAll arbitraryTcpHeader $ \ hdr ->
  let bytes = runPut (putTcpHeader hdr)
      len   = S.length bytes
   in case runGet getTcpHeader bytes of
        Left err                        -> failed { reason = err }
        Right (hdr',len') | hdr == hdr' -> succeeded
                          | otherwise   -> failed

prop_optionRoundTrip = forAll arbitraryTcpOption $ \ opt ->
  case runGet getTcpOption (runPut (putTcpOption opt)) of
    Right opt' | opt == opt' -> succeeded
               | otherwise   -> failed
    Left err                 -> failed { reason = err }
