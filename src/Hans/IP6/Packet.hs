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
import           Data.Foldable (toList)
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
import qualified Hans.Network.Types as T


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

-- |The size of the IP6 header in bytes.
ip6HeaderSize :: IP6Header -> Int
ip6HeaderSize _ = 40

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

-- IP6 Hop-By-Hop and Destination Options Headers ------------------------------
--
--  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--  |  Next Header  |  Hdr Ext Len  |                               |
--  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                               +
--  |                                                               |
--  .                                                               .
--  .                            Options                            .
--  .                                                               .
--  |                                                               |
--  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--
-- Next Header          8-bit selector.  Identifies the type of header
--                      immediately following the Hop-by-Hop Options
--                      header.  Uses the same values as the IPv4
--                      Protocol field [RFC-1700 et seq.].
--
-- Hdr Ext Len          8-bit unsigned integer.  Length of the Hop-by-
--                      Hop Options header in 8-octet units, not
--                      including the first 8 octets.
--
-- Options              Variable-length field, of length such that the
--                      complete Hop-by-Hop Options header is an integer
--                      multiple of 8 octets long.  Contains one or more
--                      TLV-encoded options, as described in section
--                      4.2.

data IP6OptionsHeader = IP6OptionsHeader {
         ohKind            :: IP6OptionsHeaderKind
       , ohNextHeader      :: Word8
       , ohExtensionLength :: Word8
       , ohOptions         :: [IP6Option]
       } deriving (Show)

data IP6OptionsHeaderKind = HopByHop | Destination
  deriving (Show)

-- |The size in bytes of the options extension header.
ip6OptionsHeaderSize :: IP6OptionsHeader -> Int
ip6OptionsHeaderSize oh = 8 + 8 * fromIntegral (ohExtensionLength oh)

getIP6HopByHop :: Get IP6OptionsHeader
getIP6HopByHop = getIP6OptionsHeader HopByHop

getIP6Destination :: Get IP6OptionsHeader
getIP6Destination = getIP6OptionsHeader Destination

getIP6OptionsHeader :: IP6OptionsHeaderKind -> Get IP6OptionsHeader
getIP6OptionsHeader ohKind =
  do ohNextHeader      <- getWord8
     ohExtensionLength <- getWord8
     let totalLength    = 8 + (fromIntegral ohExtensionLength * 8)
     label "IPv6 Hop-By-Hop Options" $
       isolate (totalLength - 2) $
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

-- Options for IP6 Hop-By-Hop Options Header and Destination Options Header ----

data IP6Option = Pad1
               | PadN                  Word8
               | UnknownSkip           Word8 S.ByteString
               | UnknownDiscard        Word8 S.ByteString
               | UnknownAlwaysSendICMP Word8 S.ByteString
               | UnknownDestSendICMP   Word8 S.ByteString
               deriving (Show)

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
               -- RFC 2460 Section 4.2.
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
--
-- The format for Type 0 Routing Headers, from RFC 2460:
--
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |  Next Header  |  Hdr Ext Len  | Routing Type=0| Segments Left |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                            Reserved                           |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +                           Address[1]                          +
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +                           Address[2]                          +
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- .                               .                               .
-- .                               .                               .
-- .                               .                               .
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +                           Address[n]                          +
-- |                                                               |
-- +                                                               +
-- |                                                               |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--
-- Next Header          8-bit selector.  Identifies the type of header
--                      immediately following the Routing header.  Uses
--                      the same values as the IPv4 Protocol field
--                      [RFC-1700 et seq.].
--
-- Hdr Ext Len          8-bit unsigned integer.  Length of the Routing
--                      header in 8-octet units, not including the first
--                      8 octets.  For the Type 0 Routing header, Hdr
--                      Ext Len is equal to two times the number of
--                      addresses in the header.
--
-- Routing Type         0.
--
--
-- The "reserved" bytes are ignored, and expected to be zero valued.
--
-- According to RFC5095 (https://tools.ietf.org/html/rfc5095): the
-- Type 0 Routing Header is deprecated, due to the possibility of
-- using it for amplification attacks. When receiving a packet with
-- this header the receiver should check if the "segments left" field
-- is zero. If it is, then ignore the header, otherwise discard the
-- packet and send an ICMP error. But implementations are not required
-- to support the Type 0 Routing Header at all.

data IP6Routing = IP6Routing {
         rhNextHeader      :: Word8
       , rhExtensionLength :: Word8
       , rhSegmentsLeft    :: Word8
       , rhRoutingData     :: IP6RoutingData
       } deriving (Show)

data IP6RoutingData = Type0RoutingData (Array Word8 IP6)
                    | UnknownRoutingData Word8 S.ByteString
                    deriving (Show)

-- |The size in bytes of the routing extension header.
ip6RoutingSize :: IP6Routing -> Int
ip6RoutingSize rh = 8 + 8 * fromIntegral (rhExtensionLength rh)

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
  do _reserved <- replicateM 4 getWord8
     let n = elen `div` 2
     addrs <- replicateM (fromIntegral n) getIP6
     return (Type0RoutingData (listArray (1,n) addrs)) -- yes, 1..n.

-- |Serialize and IPv6 Routing Extension Header.
--
-- Because the Routing Extension Header is deprecated, we don't expect
-- to originate such packets. However, when acting as a router or
-- bridge, we might serialize after deserializing a router header
-- originating elsewhere.
putIP6Routing :: IP6Routing -> Put
putIP6Routing rh =
  do putWord8 (rhNextHeader rh)
     putWord8 (rhExtensionLength rh)
     putWord8 (rhRoutingType rh)
     putWord8 (rhSegmentsLeft rh)
     case rhRoutingData rh of
       Type0RoutingData addrs      -> putType0RoutingData addrs
       UnknownRoutingData _ bytes  -> putByteString bytes
  where
    rhRoutingType rh =
      case rhRoutingData rh of
        Type0RoutingData{}     -> 0
        UnknownRoutingData t _ -> t

    putType0RoutingData addrs =
      do -- The ignored "reserved" bytes.
         _ <- replicateM 4 (putWord8 0)
         mapM_ putIP6 (toList addrs)

-- IP6 Fragmentation Header ----------------------------------------------------
--
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |  Next Header  |   Reserved    |      Fragment Offset    |Res|M|
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                         Identification                        |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

data IP6Fragment = IP6Fragment {
         frNextHeader    :: Word8
       , frOffset        :: Word16
       , frMoreFragments :: Bool
       , frIdent         :: Word32
       , frOrigin        :: IP6Header
     } deriving (Show)

-- |The size in bytes of the fragment extension header.
ip6FragmentSize :: IP6Fragment -> Int
ip6FragmentSize _ = 8

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

-- Iterated Extension Header Parsing -------------------------------------------
--
-- This overlaps with code in 'Hans.IP6.Input' that mixes iterated
-- header parsing with network stack operations.

data IP6ExtensionHeader = IP6EhOptions  IP6OptionsHeader
                        | IP6EhFragment IP6Fragment
                        | IP6EhRouting  IP6Routing
                        deriving (Show)

-- |The size in bytes of an extension header.
ip6ExtensionHeaderSize :: IP6ExtensionHeader -> Int
ip6ExtensionHeaderSize e =
  case e of
    IP6EhOptions o  -> ip6OptionsHeaderSize o
    IP6EhFragment f -> ip6FragmentSize f
    IP6EhRouting r  -> ip6RoutingSize r

-- |An IP6 header and all of the extension headers that follow it.
--
-- The last header indicates the higher level payload type, which is
-- also cached in the 'ip6Protocol' field.
data IP6Headers = IP6Headers
  { ip6Header     :: IP6Header
  , ip6Extensions :: [IP6ExtensionHeader]
  , ip6Protocol   :: NetworkProtocol
    -- ^ The next header value from the final extension
    -- header, or from the 'IP6Header' if there are no
    -- extensions headers.
  } deriving (Show)

-- |The cumulative size in bytes of the main header and all extension
-- headers.
ip6HeadersSize :: IP6Headers -> Int
ip6HeadersSize IP6Headers{..} =
  ip6HeaderSize ip6Header +
  sum (map ip6ExtensionHeaderSize ip6Extensions)

-- |Parse an IP6 packet, *including all extension headers*.
--
-- Returns headers, cumulative length of headers, and length of the
-- remaining payload (should be transport layer or ICMP?).
--
-- Contrast with 'getIP6Packet', which only parses the initial,
-- non-extension header.
getIP6Packet' :: Get (IP6Headers, Int, Int)
getIP6Packet' =
  label "IP6 Extension Headers" $
    do (ip6Header,_hdrLen,bodyLen) <- getIP6Packet
       (ip6Extensions,ip6Protocol) <- getIP6Extensions ip6Header
       let hdrs     = IP6Headers{..}
       let hdrsLen  = ip6HeadersSize hdrs
       let bodyLen' = bodyLen - hdrsLen
       return (hdrs,hdrsLen,bodyLen')

-- |Parse IP6 extension headers until there is no next header, or the
-- next header is not an extension (transport layer or ICMP).
--
-- Returns the extension headers, and the payload protocol.
getIP6Extensions ::
  IP6Header -> Get ([IP6ExtensionHeader], NetworkProtocol)
getIP6Extensions hdr@(IP6Header{..}) = go [] ip6NextHeader
  where
    go headerStack nextHeader =
      case nextHeader of
        T.PROT_UDP         -> done
        T.PROT_TCP         -> done
        T.PROT_IP6_ICMP    -> done
        T.PROT_IP6_NO_NEXT -> done

        T.PROT_IP6_HOP_BY_HOP ->
          do oh <- getIP6HopByHop
             go (IP6EhOptions oh : headerStack) (ohNextHeader oh)
        T.PROT_IP6_DEST_OPTS ->
          do oh <- getIP6Destination
             go (IP6EhOptions oh : headerStack) (ohNextHeader oh)
        T.PROT_IP6_FRAGMENT ->
          do fr <- getIP6Fragment hdr
             go (IP6EhFragment fr : headerStack) (frNextHeader fr)
        T.PROT_IP6_ROUTING ->
          do rh <- getIP6Routing
             go (IP6EhRouting rh : headerStack) (rhNextHeader rh)
             -- There are several other protocols defined in
             -- 'Hans.Network.Types'. We can support them as the need
             -- arises ...
        _ -> error $ "getIP6Extensions: unsupported next header: " ++
                     show nextHeader ++ " (" ++
                     show (T.maybeShowProtocol nextHeader) ++ ")"
      where
        done = return (reverse headerStack, nextHeader)
