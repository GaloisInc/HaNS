{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tcp.Packet where

import Hans.Message.Tcp (TcpHeader(..),emptyTcpHeader)

import Control.Monad (replicateM)
import Test.QuickCheck (Gen,choose,arbitrarySizedIntegral,sized)
import qualified Data.ByteString as S
import qualified Data.Sequence as Seq


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
