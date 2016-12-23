{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Hans.IP6.Packet where

import           Control.Monad (unless, replicateM)
import           Data.Array(Array,listArray)
import           Data.Bits((.|.),(.&.),shiftL,shiftR,testBit)
import qualified Data.ByteString as S
import           Data.Serialize
                    (Get,getWord8,getWord16be,getWord32be,getByteString
                    ,label,isolate,skip,runPut,isEmpty,getBytes
                    ,Put,putWord8,putWord16be,putWord32be,putByteString)
import           Data.Word (Word8,Word16,Word32)
import           Hans.Addr (IP6,getIP6,putIP6,packIP6)
import           Hans.Checksum
                     (Checksum(..),PartialChecksum,emptyPartialChecksum)
import           Hans.Lens as L
import           Hans.Network.Fragments(Fragmentable(..))
import           Hans.Network.Types (NetworkProtocol)


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

-- IP6 Hop-By-Hop Options Header -----------------------------------------------

data IP6OptionsHeader = IP6OptionsHeader {
         ohKind            :: IP6OptionsHeaderKind
       , ohNextHeader      :: Word8
       , ohExtensionLength :: Integer
         -- ^In bytes, including the whole packet
       , ohOptions         :: [IP6Option]
       }

data IP6OptionsHeaderKind = HopByHop | Destination

getIP6OptionsHeader :: IP6OptionsHeaderKind -> Get IP6OptionsHeader
getIP6OptionsHeader ohKind =
  do ohNextHeader <- getWord8
     lenField     <- getWord8
     let ohExtensionLength = 8 + (fromIntegral lenField * 8)
     label "IPv6 Hop-By-Hop Options" $
       isolate (fromIntegral ohExtensionLength - 2) $
         do ohOptions <- getOptions
            return IP6OptionsHeader{..}

putIP6HopByHop :: IP6OptionsHeader -> Put
putIP6HopByHop oh =
  do let optsbstr      = map (runPut . putOption) (ohOptions oh)
         optbstr       = S.concat optsbstr
         baselen       = S.length optbstr + 2
         padLen        = 8 - (baselen `mod` 8)
         padLen'       = if padLen == 8 then 0 else padLen
         padding       = buildOptionPadding padLen'
         len           = baselen + padLen'
     putWord8 (ohNextHeader oh)
     putWord8 (fromIntegral ((len `div` 8) - 1))
     putByteString optbstr
     mapM_ putOption padding

-- IP6 Hop-By-Hop Options Header -----------------------------------------------

data IP6Option = Pad1
               | PadN                  Word8
               | UnknownSkip           Word8 S.ByteString
               | UnknownDiscard        Word8 S.ByteString
               | UnknownAlwaysSendICMP Word8 S.ByteString
               | UnknownDestSendICMP   Word8 S.ByteString

getOptions :: Get [IP6Option]
getOptions =
  do done <- isEmpty
     if done
       then return []
       else do first <- getOption
               (first:) `fmap` getOptions

getOption :: Get IP6Option
getOption =
  do optionType <- getWord8
     case optionType of
       0 -> return Pad1
       1 -> do len <- getWord8
               skip (fromIntegral len)
               return (PadN len)
       _ -> do len  <- getWord8
               bstr <- getByteString (fromIntegral len)
               case optionType `shiftR` 6 of
                 0 -> return (UnknownSkip           optionType bstr)
                 1 -> return (UnknownDiscard        optionType bstr)
                 2 -> return (UnknownAlwaysSendICMP optionType bstr)
                 3 -> return (UnknownDestSendICMP   optionType bstr)
                 _ -> error "All of computing is a lie."

putOption :: IP6Option -> Put
putOption opt =
  case opt of
    Pad1                      -> putWord8   0
    PadN                  l   -> putOption' 1 (S.replicate (fromIntegral l) 0)
    UnknownSkip           t s -> putOption' t s
    UnknownDiscard        t s -> putOption' t s
    UnknownAlwaysSendICMP t s -> putOption' t s
    UnknownDestSendICMP   t s -> putOption' t s
 where
  putOption' t s =
    do putWord8 t
       putWord8 (fromIntegral (S.length s))
       putByteString s

buildOptionPadding :: Int -> [IP6Option]
buildOptionPadding 0 = []
buildOptionPadding 1 = [Pad1]
buildOptionPadding x
  | x < 0     = []
  | x <= 257  = [PadN (fromIntegral x - 2)]
  | otherwise = (PadN (fromIntegral x - 2)) : buildOptionPadding (x - 257)

-- IP6 Routing Header ----------------------------------------------------------

data IP6Routing = IP6Routing {
         rhNextHeader      :: Word8
       , rhExtensionLength :: Word8
       , rhSegmentsLeft    :: Word8
       , rhRoutingData     :: IP6RoutingData
       }

data IP6RoutingData = Type0RoutingData (Array Word8 IP6)
                    | UnknownRoutingData Word8 S.ByteString

rhAddresses :: IP6Routing -> Maybe (Array Word8 IP6)
rhAddresses IP6Routing{ rhRoutingData = Type0RoutingData x } = Just x
rhAddresses _                                                = Nothing

getIP6Routing :: Get IP6Routing
getIP6Routing  =
  do rhNextHeader      <- getWord8
     rhExtensionLength <- getWord8
     rhRoutingType     <- getWord8
     rhSegmentsLeft    <- getWord8
     let totalLen = 8 + (rhExtensionLength * 8)
         restLen  = fromIntegral (totalLen - 4)
     label "IP6 Routing Data" $
       isolate restLen $
         do rhRoutingData <-
              case rhRoutingType of
                0 -> getType0RoutingData rhExtensionLength
                _ -> UnknownRoutingData rhRoutingType <$> getBytes restLen
            return IP6Routing{..}

getType0RoutingData :: Word8 -> Get IP6RoutingData
getType0RoutingData elen =
  do let n = elen `div` 2
     stuff <- replicateM (fromIntegral n) getIP6
     return (Type0RoutingData (listArray (1,n) stuff)) -- yes, 1..n.

-- IP6 Fragmentation Header ----------------------------------------------------

data IP6Fragment = IP6Fragment {
         frNextHeader    :: Word8
       , frOffset        :: Word16
       , frMoreFragments :: Bool
       , frIdent         :: Word32
       , frOrigin        :: IP6Header
     }

getIP6Fragment :: IP6Header -> Get IP6Fragment
getIP6Fragment frOrigin =
  do frNextHeader <- getWord8
     _            <- getWord8
     bleh         <- getWord16be
     frIdent      <- getWord32be
     let frMoreFragments = testBit bleh 0
         frOffset        = bleh `shiftR` 3
     return IP6Fragment{..}

putIP6Fragment :: IP6Fragment -> Put
putIP6Fragment fr =
  do putWord8 (frNextHeader fr)
     putWord8 0
     let shiftedOffset = frOffset fr `shiftL` 4
         offsetplus | frMoreFragments fr = shiftedOffset .|. 1
                    | otherwise          = shiftedOffset
     putWord16be offsetplus
     putWord32be (frIdent fr)

instance Fragmentable IP6 Word32 IP6Fragment where
  mkKey hdr = (ip6SourceAddr (frOrigin hdr),
               ip6DestAddr   (frOrigin hdr),
               ip6NextHeader (frOrigin hdr),
               frIdent       hdr)
  hdrMoreFragments = frMoreFragments
  hdrFragOffset = frOffset

