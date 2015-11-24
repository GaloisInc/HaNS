{-# LANGUAGE RecordWildCards #-}

module Tests.IP4.Packet where

import Tests.Ethernet (arbitraryMac)
import Tests.Utils (encodeDecodeIdentity)

import Hans.IP4.Packet

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as Sh
import           Data.Word (Word8)
import           Test.QuickCheck
import           Test.Tasty (testGroup,TestTree)
import           Test.Tasty.QuickCheck (testProperty)


-- Packet Generator Support ----------------------------------------------------

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


arbitraryPayload :: Int -> Gen L.ByteString
arbitraryPayload len =
  do bytes <- vectorOf len arbitraryBoundedRandom
     return (L.pack bytes)

arbitraryOptionPayload :: Word8 -> Gen Sh.ShortByteString
arbitraryOptionPayload len =
  do bytes <- vectorOf (fromIntegral len) arbitraryBoundedRandom
     return (Sh.pack bytes)


arbitraryIP4Header :: Gen IP4Header
arbitraryIP4Header  =
  do ip4TypeOfService <- arbitraryBoundedRandom
     ip4Ident         <- arbitraryIdent
     ip4DontFragment  <- arbitraryBoundedRandom
     ip4MoreFragments <- arbitraryBoundedRandom

     -- offset MUST always be a multiple of 8
     off <- choose (0,0x1fff)
     let ip4FragmentOffset = off * 8

     ip4TimeToLive    <- arbitraryBoundedRandom
     ip4Protocol      <- arbitraryProtocol
     ip4SourceAddr    <- arbitraryIP4
     ip4DestAddr      <- arbitraryIP4

     -- checksum processing is validated by a different property
     let ip4Checksum = 0

     -- XXX need to generate options that fit within the additional 40 bytes
     -- available
     let ip4Options = []

     return IP4Header { .. }


arbitraryIP4Option :: Gen IP4Option
arbitraryIP4Option  =
  do ip4OptionCopied  <- arbitraryBoundedRandom
     ip4OptionClass   <- choose (0,0x3)
     ip4OptionNum     <- choose (0,0x1f)

     ip4OptionData <-
       if ip4OptionNum < 2
          then return Sh.empty
          else do len <- choose (0, 0xff - 2)
                  arbitraryOptionPayload len

     return IP4Option { .. }


arbitraryArpPacket :: Gen ArpPacket
arbitraryArpPacket  =
  do arpOper <- elements [ArpRequest,ArpReply]
     arpSHA  <- arbitraryMac
     arpSPA  <- arbitraryIP4
     arpTHA  <- arbitraryMac
     arpTPA  <- arbitraryIP4
     return ArpPacket { .. }


-- Packet Properties -----------------------------------------------------------

packetTests :: TestTree
packetTests  = testGroup "Packet"
  [ testProperty "IP4 Address encode/decode" $
    encodeDecodeIdentity putIP4 getIP4 arbitraryIP4

  , let get =
          do (hdr,20,0) <- getIP4Packet
             return hdr

        put hdr =
             putIP4Header hdr 0

     in testProperty "Header encode/decode" $
        encodeDecodeIdentity put get arbitraryIP4Header

  , testProperty "Option encode/decode" $
    encodeDecodeIdentity putIP4Option getIP4Option arbitraryIP4Option

  , testProperty "Arp Message encode/decode" $
    encodeDecodeIdentity putArpPacket getArpPacket arbitraryArpPacket
  ]
