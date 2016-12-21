{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Hans.IP6.Packet where

import Hans.Addr (IP6,getIP6,putIP6,packIP6)
import Hans.Checksum
           (Checksum(..),PartialChecksum,emptyPartialChecksum)
import Hans.Lens as L
import Hans.Network.Types (NetworkProtocol)

import Control.Monad (unless)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Data.Serialize
          (Get,getWord8,getWord16be,getWord32be
          ,label,isolate
          ,Put,putWord8,putWord16be,putWord32be)
import Data.Word (Word8,Word32)


-- IP6 Pseudo Header -----------------------------------------------------------

--   0                                                              32
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   +                                                               +
--   |                                                               |
--   +                         Source Address                        +
--   |                                                               |
--   +                                                               +
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   +                                                               +
--   |                                                               |
--   +                      Destination Address                      +
--   |                                                               |
--   +                                                               +
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                   Upper-Layer Packet Length                   |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                      zero                     |  Next Header  |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


ip6PseudoHeader :: IP6 -> IP6 -> NetworkProtocol -> Int -> PartialChecksum
ip6PseudoHeader src dst prot len =
  extendChecksum (fromIntegral prot :: Word32) $
  extendChecksum (fromIntegral len  :: Word32) $
  extendChecksum dst                           $
  extendChecksum src emptyPartialChecksum

-- IP6 Packets -----------------------------------------------------------------

-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |Version| Traffic Class |           Flow Label                  |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |         Payload Length        |  Next Header  |   Hop Limit   |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +                         Source Address                        +
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +                      Destination Address                      +
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

data IP6Header = IP6Header
  { ip6TrafficFlow   :: {-# UNPACK #-} !Word32
    -- ^ This includes the version, traffic class, and flow label
  , ip6NextHeader    :: {-# UNPACK #-} !NetworkProtocol
  , ip6HopLimit      :: {-# UNPACK #-} !Word8
  , ip6SourceAddr    :: {-# UNPACK #-} !IP6
  , ip6DestAddr      :: {-# UNPACK #-} !IP6
  } deriving (Eq,Show)

emptyIP4Header :: IP6Header
emptyIP4Header  = IP6Header
  { ip6TrafficFlow   = 6 `shiftL` 28
  , ip6NextHeader    = 0
  , ip6HopLimit      = 255
  , ip6SourceAddr    = packIP6 0 0
  , ip6DestAddr      = packIP6 0 0
  }

ip6TrafficClass :: Lens' IP6Header Word8
ip6TrafficClass f IP6Header{..} =
  fmap (\ w -> IP6Header {
          ip6TrafficFlow = ip6TrafficFlow .|. (fromIntegral w `shiftL` 20), ..})
       (f (fromIntegral (ip6TrafficFlow `shiftR` 20)))
{-# INLINE ip6TrafficClass #-}

ip6FlowLabel :: Lens' IP6Header Word32
ip6FlowLabel f IP6Header{..} =
  fmap (\ w -> IP6Header { ip6TrafficFlow = ip6TrafficFlow .|. (w .&. 0xFFFFF), ..})
       (f (ip6TrafficFlow .&. 0xFFFFF))
{-# INLINE ip6FlowLabel #-}

-- |Parse an IP6 Header, returning the header, the header length, and the length
-- of the payload.
getIP6Packet :: Get (IP6Header, Int, Int)
getIP6Packet  =
  label "IP6 Header" $
    isolate 40 $
      do ip6TrafficFlow   <- getWord32be
         let ver = ip6TrafficFlow `shiftR` 28
         unless (ver == 6) (fail "Invalid version")
         payloadLength    <- getWord16be
         ip6NextHeader    <- getWord8
         ip6HopLimit      <- getWord8
         ip6SourceAddr    <- getIP6
         ip6DestAddr      <- getIP6
         return (IP6Header{..}, 40, fromIntegral payloadLength)

putIP6Header :: IP6Header -> Int -> Put
putIP6Header IP6Header { .. } pktlen =
  do putWord32be ip6TrafficFlow
     putWord16be (fromIntegral pktlen)
     putWord8    ip6NextHeader
     putWord8    ip6HopLimit
     putIP6      ip6SourceAddr
     putIP6      ip6DestAddr


type IP6Ident = Word32
