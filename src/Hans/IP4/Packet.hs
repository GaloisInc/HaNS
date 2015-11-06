{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.IP4.Packet where

import Hans.Checksum (computeChecksumLazy)
import Hans.Ethernet (Mac,getMac,putMac,pattern ETYPE_IPV4)
import Hans.Serialize (runPutPacket)

import           Control.Monad (unless,guard)
import           Data.Bits ((.|.),(.&.),testBit,shiftL,shiftR,bit,setBit)
import qualified Data.ByteString.Short as Sh
import qualified Data.ByteString.Lazy as L
import           Data.Hashable (Hashable)
import           Data.Int (Int64)
import           Data.Serialize
                    (Get,getWord8,getWord16be,getWord32be,getShortByteString
                    ,label,isolate
                    ,Putter,Put,putWord8,putWord16be,putWord32be
                    ,putLazyByteString,putShortByteString)
import           Data.Word (Word8,Word16,Word32)


-- IP4 Addresses ---------------------------------------------------------------

newtype IP4 = IP4 Word32
              deriving (Eq,Ord,Show,Hashable)

getIP4 :: Get IP4
getIP4  =
  do w <- getWord32be
     return (IP4 w)

putIP4 :: Putter IP4
putIP4 (IP4 w) = putWord32be w

packIP4 :: Word8 -> Word8 -> Word8 -> Word8 -> IP4
packIP4 a b c d = IP4 $ fromIntegral a `shiftL` 24
                    .|. fromIntegral b `shiftL` 16
                    .|. fromIntegral c `shiftL`  8
                    .|. fromIntegral d

unpackIP4 :: IP4 -> (Word8,Word8,Word8,Word8)
unpackIP4 (IP4 w) = ( fromIntegral (w `shiftR` 24)
                    , fromIntegral (w `shiftR` 16)
                    , fromIntegral (w `shiftR`  8)
                    , fromIntegral  w
                    )


-- IP4 Pseudo Header -----------------------------------------------------------

-- 0      7 8     15 16    23 24    31 
-- +--------+--------+--------+--------+
-- |          source address           |
-- +--------+--------+--------+--------+
-- |        destination address        |
-- +--------+--------+--------+--------+
-- |  zero  |protocol|     length      |
-- +--------+--------+--------+--------+
mkIP4PseudoHeader :: IP4 -> IP4 -> IP4Protocol -> Int -> L.ByteString
mkIP4PseudoHeader src dst prot len = runPutPacket 3 10 L.empty $ do
  putIP4 src
  putIP4 dst
  putWord8 0 >> putWord8 prot >> putWord16be (fromIntegral len)


-- IP4 Packets -----------------------------------------------------------------

type IP4Ident = Word16

type IP4Protocol = Word8

pattern IP4_PROT_ICMP = 0x1
pattern IP4_PROT_TCP  = 0x6
pattern IP4_PROT_UDP  = 0x11


data IP4Header = IP4Header
  { ip4TypeOfService  :: {-# UNPACK #-} !Word8
  , ip4Ident          :: {-# UNPACK #-} !IP4Ident
  , ip4DontFragment   :: !Bool
  , ip4MoreFragments  :: !Bool
  , ip4FragmentOffset :: {-# UNPACK #-} !Word16
  , ip4TimeToLive     :: {-# UNPACK #-} !Word8
  , ip4Protocol       :: {-# UNPACK #-} !IP4Protocol
  , ip4Checksum       :: {-# UNPACK #-} !Word16
  , ip4SourceAddr     :: {-# UNPACK #-} !IP4
  , ip4DestAddr       :: {-# UNPACK #-} !IP4
  , ip4Options        :: ![IP4Option]
  } deriving (Eq,Show)

emptyIP4Header :: IP4Header
emptyIP4Header  = IP4Header
  { ip4TypeOfService  = 0
  , ip4Ident          = 0
  , ip4DontFragment   = False
  , ip4MoreFragments  = False
  , ip4FragmentOffset = 0
  , ip4TimeToLive     = 127
  , ip4Protocol       = 0
  , ip4Checksum       = 0
  , ip4SourceAddr     = IP4 0
  , ip4DestAddr       = IP4 0
  , ip4Options        = []
  }


noMoreFragments :: IP4Header -> IP4Header
noMoreFragments hdr = hdr { ip4MoreFragments = False }

moreFragments :: IP4Header -> IP4Header
moreFragments hdr = hdr { ip4MoreFragments = True }

addOffset :: Word16 -> IP4Header -> IP4Header
addOffset off hdr = hdr { ip4FragmentOffset = ip4FragmentOffset hdr + off }

setIdent :: IP4Ident -> IP4Header -> IP4Header
setIdent i hdr = hdr { ip4Ident = i }


-- | Calculate the size of an IP4 packet
ip4PacketSize :: IP4Header -> L.ByteString -> Int
ip4PacketSize hdr bs =
  ip4HeaderSize hdr + fromIntegral (L.length bs)

-- | Calculate the size of an IP4 header
ip4HeaderSize :: IP4Header -> Int
ip4HeaderSize hdr = 20 + sum (map ip4OptionSize (ip4Options hdr))


-- | Fragment a single IP packet into one or more, given an MTU to fit into.
splitPacket :: Int -> IP4Header -> L.ByteString -> [(IP4Header,L.ByteString)]
splitPacket mtu hdr bs
  | ip4PacketSize hdr bs <= mtu = [(hdr,bs)]
  | otherwise                   = fragmentPacket (fromIntegral mtu) hdr bs


-- | Given a fragment size and a packet, fragment the packet into multiple
-- smaller ones.
fragmentPacket :: Int64 -> IP4Header -> L.ByteString
               -> [(IP4Header,L.ByteString)]
fragmentPacket mtu = loop
  where
  loop hdr bs
    | payloadLen <= mtu = [(noMoreFragments hdr, bs)]
    | otherwise         = frag : loop hdr' rest
    where
    payloadLen = L.length bs
    (as,rest)  = L.splitAt mtu bs
    alen       = fromIntegral (L.length as)
    hdr'       = addOffset alen hdr
    frag       = (moreFragments hdr, as)


--  0                   1                   2                   3   
--  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |Version|  IHL  |Type of Service|          Total Length         |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |         Identification        |Flags|      Fragment Offset    |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |  Time to Live |    Protocol   |         Header Checksum       |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                       Source Address                          |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |                    Destination Address                        |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
getIP4Packet :: Get (IP4Header, Int, Int)
getIP4Packet  = label "IP4 Header" $ do
  do b0 <- getWord8
     let ver = b0 `shiftR` 4
     unless (ver == 4) (fail "Invalid version")

     let ihl = fromIntegral ((b0 .&. 0xf) * 4)
     isolate (ihl - 1) $
       do ip4TypeOfService   <- getWord8
          payloadLen         <- getWord16be
          ip4Ident           <- getWord16be

          s1 <- getWord16be
          let flags             = s1 `shiftR` 13
              ip4DontFragment   = flags `testBit` 1
              ip4MoreFragments  = flags `testBit` 0
              ip4FragmentOffset = (s1 .&. 0x1fff) * 8

          ip4TimeToLive <- getWord8
          ip4Protocol   <- getWord8
          ip4Checksum   <- getWord16be
          ip4SourceAddr <- getIP4
          ip4DestAddr   <- getIP4
          let optlen = ihl - 20
          ip4Options <-
              label "IP4 Options" $ isolate optlen
                                  $ getIP4Options optlen

          let hdr = IP4Header { .. }
          hdr `seq` return (hdr, ihl, fromIntegral payloadLen)

putIP4Header :: IP4Header -> Int -> Put
putIP4Header IP4Header { .. } pktlen = do
  let (optbs,optlen) = renderIP4Options ip4Options
  let ihl            = 20 + optlen
  putWord8    (4 `shiftL` 4 .|. fromIntegral (ihl `div` 4))
  putWord8     ip4TypeOfService

  putWord16be (fromIntegral (pktlen + ihl))
  putWord16be ip4Ident

  let frag      | ip4DontFragment = (`setBit` 1)
                | otherwise       = id

      morefrags | ip4MoreFragments = (`setBit` 0)
                | otherwise        = id

      flags = frag (morefrags 0)

      off   = ip4FragmentOffset `div` 8
  putWord16be (flags `shiftL` 13 .|. off .&. 0x1fff)

  putWord8    ip4TimeToLive
  putWord8    ip4Protocol
  putWord16be 0 -- checksum

  putIP4 ip4SourceAddr
  putIP4 ip4DestAddr

  putLazyByteString optbs


-- | The final step to render an IP header and its payload out as a lazy
-- 'ByteString'. Compute the checksum over the packet with its checksum zeroed,
-- then reconstruct a new lazy 'ByteString' that contains chunks from the old
-- header, and the new checksum.
renderIP4Packet :: IP4Header -> L.ByteString -> L.ByteString
renderIP4Packet hdr pkt = newHeader `L.append` pkt
  where

  pktlen    = fromIntegral (L.length pkt)
  bytes     = runPutPacket 20 40 L.empty (putIP4Header hdr pktlen)
  cs        = computeChecksumLazy 0 bytes

  beforeCS  = L.take 10 bytes
  afterCS   = L.drop 12 bytes
  csBytes   = runPutPacket 2 100 afterCS (putWord16be cs)

  newHeader = beforeCS `L.append` csBytes


-- IP4 Options -----------------------------------------------------------------

renderIP4Options :: [IP4Option] -> (L.ByteString,Int)
renderIP4Options []   = (L.empty,0)
renderIP4Options opts =
  case optlen `mod` 4 of
    0 -> (optbs,optlen)

    -- pad with no-ops
    n -> (optbs `L.append` L.replicate (fromIntegral n) 0x1, optlen + n)
  where
  optbs  = runPutPacket 40 100 L.empty (mapM_ putIP4Option opts)
  optlen = fromIntegral (L.length optbs)


getIP4Options :: Int -> Get [IP4Option]
getIP4Options len
  | len <= 0  = return []
  | otherwise = do o    <- getIP4Option
                   rest <- getIP4Options (len - ip4OptionSize o)
                   return $! (o : rest)


data IP4Option = IP4Option
  { ip4OptionCopied :: !Bool
  , ip4OptionClass  :: {-# UNPACK #-} !Word8
  , ip4OptionNum    :: {-# UNPACK #-} !Word8
  , ip4OptionData   :: {-# UNPACK #-} !Sh.ShortByteString
  } deriving (Eq,Show)


ip4OptionSize :: IP4Option -> Int
ip4OptionSize opt = case ip4OptionNum opt of
  0 -> 1
  1 -> 1
  _ -> 2 + fromIntegral (Sh.length (ip4OptionData opt))


getIP4Option :: Get IP4Option
getIP4Option =
  do b <- getWord8
     let ip4OptionCopied = testBit b 7
     let ip4OptionClass  = (b `shiftR` 5) .&. 0x3
     let ip4OptionNum    = b .&. 0x1f

     ip4OptionData <-
       if ip4OptionNum < 2
          then return Sh.empty
          else do len <- getWord8
                  unless (len >= 2) (fail "Option length parameter is to small")
                  getShortByteString (fromIntegral (len - 2))

     return $! IP4Option { .. }

putIP4Option :: Putter IP4Option
putIP4Option IP4Option { .. } =
  do let copied | ip4OptionCopied = bit 7
                | otherwise       = 0

     putWord8 $ copied .|. ((ip4OptionClass .&. 0x3) `shiftL` 5)
                       .|.  ip4OptionNum    .&. 0x1f

     case ip4OptionNum of
       0 -> return ()
       1 -> return ()
       _ -> do putWord8 (fromIntegral (Sh.length ip4OptionData))
               putShortByteString ip4OptionData


-- Arp Packets -----------------------------------------------------------------

-- | Arp packets, specialized to IP4 and Mac addresses.
data ArpPacket = ArpPacket { arpOper   :: {-# UNPACK #-} !ArpOper
                           , arpSHA    :: !Mac
                           , arpSPA    :: !IP4
                           , arpTHA    :: !Mac
                           , arpTPA    :: !IP4
                           } deriving (Show)

-- | Parse an Arp packet, given a way to parse hardware and protocol addresses.
getArpPacket :: Get ArpPacket
getArpPacket  = label "ArpPacket" $
  do hwtype <- getWord16be
     ptype  <- getWord16be
     hwlen  <- getWord8
     plen   <- getWord8

     -- make sure that this packet is specialized to IP4/Ethernet
     guard $ hwtype == 0x1        && hwlen == 6
          && ptype  == ETYPE_IPV4 && plen  == 4

     arpOper   <- getArpOper

     arpSHA    <- getMac
     arpSPA    <- getIP4

     arpTHA    <- getMac
     arpTPA    <- getIP4

     return ArpPacket { .. }

-- | Render an Arp packet, given a way to render hardware and protocol
-- addresses.
putArpPacket :: Putter ArpPacket
putArpPacket ArpPacket { .. } =
  do putWord16be   0x1
     putWord16be   ETYPE_IPV4
     putWord8      6
     putWord8      4

     putArpOper    arpOper

     putMac        arpSHA
     putIP4        arpSPA

     putMac        arpTHA
     putIP4        arpTPA


-- Arp Opcodes -----------------------------------------------------------------

type ArpOper = Word16

pattern ArpRequest = 0x1
pattern ArpReply   = 0x2

-- | Parse an Arp operation.
getArpOper :: Get ArpOper
getArpOper  =
  do w <- getWord16be
     guard (w == ArpRequest || w == ArpReply)
     return w
{-# INLINE getArpOper #-}

-- | Render an Arp operation.
putArpOper :: Putter ArpOper
putArpOper  = putWord16be
{-# INLINE putArpOper #-}
