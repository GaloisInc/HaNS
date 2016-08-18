{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Hans.IP4.Packet where

import Hans.Addr (IP4,getIP4,putIP4)
import Hans.Checksum
           (Checksum(..),PartialChecksum,Pair8(..),emptyPartialChecksum)
import Hans.Ethernet (Mac,getMac,putMac,pattern ETYPE_IPV4)
import Hans.Lens as L
import Hans.Network.Types (NetworkProtocol)
import Hans.Serialize (runPutPacket)

import           Control.Monad (unless,guard)
import           Data.Bits as B
                     ((.|.),(.&.),testBit,shiftL,shiftR,bit,setBit,bit
                     ,complement)
import qualified Data.ByteString.Short as Sh
import qualified Data.ByteString.Lazy as L
import           Data.Hashable (Hashable)
import           Data.Int (Int64)
import           Data.Serialize
                    (Get,getWord8,getWord16be,getWord32be,getShortByteString
                    ,label,isolate,Serialize(..)
                    ,Putter,Put,putWord8,putWord16be,putWord32be
                    ,putLazyByteString,putShortByteString)
import           Data.Typeable (Typeable)
import           Data.Word (Word8,Word16,Word32)
import           GHC.Generics (Generic)


-- IP4 Pseudo Header -----------------------------------------------------------

-- 0      7 8     15 16    23 24    31 
-- +--------+--------+--------+--------+
-- |          source address           |
-- +--------+--------+--------+--------+
-- |        destination address        |
-- +--------+--------+--------+--------+
-- |  zero  |protocol|     length      |
-- +--------+--------+--------+--------+
ip4PseudoHeader :: IP4 -> IP4 -> NetworkProtocol -> Int -> PartialChecksum
ip4PseudoHeader src dst prot len =
  extendChecksum (fromIntegral len :: Word16) $
  extendChecksum (Pair8 0 prot)               $
  extendChecksum dst                          $
  extendChecksum src emptyPartialChecksum


-- IP4 Packets -----------------------------------------------------------------

type IP4Ident = Word16


data IP4Header = IP4Header
  { ip4TypeOfService  :: {-# UNPACK #-} !Word8
  , ip4Ident          :: {-# UNPACK #-} !IP4Ident
  , ip4Fragment_      :: {-# UNPACK #-} !Word16
    -- ^ This includes the flags, and the fragment offset.
  , ip4TimeToLive     :: {-# UNPACK #-} !Word8
  , ip4Protocol       :: {-# UNPACK #-} !NetworkProtocol
  , ip4Checksum       :: {-# UNPACK #-} !Word16
  , ip4SourceAddr     :: {-# UNPACK #-} !IP4
  , ip4DestAddr       :: {-# UNPACK #-} !IP4
  , ip4Options        :: ![IP4Option]
  } deriving (Eq,Show)

emptyIP4Header :: IP4Header
emptyIP4Header  = IP4Header
  { ip4TypeOfService  = 0
  , ip4Ident          = 0
  , ip4Fragment_      = 0
  , ip4TimeToLive     = 127
  , ip4Protocol       = 0
  , ip4Checksum       = 0
  , ip4SourceAddr     = IP4 0
  , ip4DestAddr       = IP4 0
  , ip4Options        = []
  }

ip4DCSP :: Lens' IP4Header Word8
ip4DCSP f IP4Header { .. } =
  fmap (\ w -> IP4Header { ip4TypeOfService = ip4TypeOfService .|. (w `shiftL` 2), .. })
       (f (ip4TypeOfService `shiftR` 2))
{-# INLINE ip4DCSP #-}

ip4ECN :: Lens' IP4Header Word8
ip4ECN f IP4Header { .. } =
  fmap (\ w -> IP4Header { ip4TypeOfService = ip4TypeOfService .|. (w .&. 0x3), .. })
       (f (ip4TypeOfService .&. 0x3))
{-# INLINE ip4ECN #-}

ip4Fragment :: Lens' IP4Header Word16
ip4Fragment f IP4Header { .. } =
  fmap (\flags' -> IP4Header { ip4Fragment_ = flags', .. }) (f ip4Fragment_)
{-# INLINE ip4Fragment #-}

ip4DontFragment :: Lens' IP4Header Bool
ip4DontFragment  = ip4Fragment . L.bit 14
{-# INLINE ip4DontFragment #-}

ip4MoreFragments :: Lens' IP4Header Bool
ip4MoreFragments  = ip4Fragment . L.bit 13
{-# INLINE ip4MoreFragments #-}

-- | The fragment offset, in bytes.
ip4FragmentOffset :: Lens' IP4Header Word16
ip4FragmentOffset  = ip4Fragment . lens f g
  where
  f frag     = (frag .&. 0x1fff) `shiftL` 3
  g frag len = (frag .&. complement 0x1fff)
           .|. ((len `shiftR` 3) .&. 0x1fff)
{-# INLINE ip4FragmentOffset #-}


noMoreFragments :: IP4Header -> IP4Header
noMoreFragments  = set ip4MoreFragments False

moreFragments :: IP4Header -> IP4Header
moreFragments  = set ip4MoreFragments True

addOffset :: Word16 -> IP4Header -> IP4Header
addOffset off = over ip4FragmentOffset (+ off)

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
fragmentPacket mtu0 hdr0 = loop hdr0
  where
  mtu = mtu0 - fromIntegral (ip4HeaderSize hdr0)

  loop hdr bs
    | payloadLen <= mtu = [(noMoreFragments hdr, bs)]
    | otherwise         = frag : loop hdr' rest
    where
    payloadLen = L.length bs
    (as,rest)  = L.splitAt mtu bs
    alen       = fromIntegral (L.length as)
    hdr'       = addOffset alen hdr
    frag       = (moreFragments hdr, as)


-- | Compute the value of the version/header length byte.
ip4VersionIHL :: Int -> Word8
ip4VersionIHL ihl = 4 `shiftL` 4 .|. fromIntegral (ihl `shiftR` 2)


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
          ip4Fragment_       <- getWord16be
          ip4TimeToLive      <- getWord8
          ip4Protocol        <- getWord8
          ip4Checksum        <- getWord16be
          ip4SourceAddr      <- getIP4
          ip4DestAddr        <- getIP4
          let optlen = ihl - 20
          ip4Options <-
              label "IP4 Options" $ isolate optlen
                                  $ getIP4Options optlen

          let hdr = IP4Header { .. }
          hdr `seq` return (hdr, ihl, fromIntegral payloadLen - ihl)

putIP4Header :: IP4Header -> Int -> Put
putIP4Header IP4Header { .. } pktlen = do
  let (optbs,optlen) = renderIP4Options ip4Options
  let ihl            = 20 + optlen
  putWord8    (ip4VersionIHL ihl)
  putWord8     ip4TypeOfService

  putWord16be (fromIntegral (pktlen + ihl))
  putWord16be ip4Ident
  putWord16be ip4Fragment_

  putWord8    ip4TimeToLive
  putWord8    ip4Protocol
  putWord16be 0 -- checksum

  putIP4 ip4SourceAddr
  putIP4 ip4DestAddr

  putLazyByteString optbs


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


ip4OptionType :: Bool -> Word8 -> Word8 -> Word8
ip4OptionType copied cls num =
  copiedFlag ((cls .&. 0x3 `shiftL` 5) .|. (num .&. 0x1f))
  where
  copiedFlag | copied    = (`setBit` 7)
             | otherwise = id

putIP4Option :: Putter IP4Option
putIP4Option IP4Option { .. } =
  do let copied | ip4OptionCopied = B.bit 7
                | otherwise       = 0

     putWord8 $ copied .|. ((ip4OptionClass .&. 0x3) `shiftL` 5)
                       .|.  ip4OptionNum    .&. 0x1f

     case ip4OptionNum of
       0 -> return ()
       1 -> return ()
       _ -> do putWord8 (fromIntegral (Sh.length ip4OptionData + 2))
               putShortByteString ip4OptionData


-- Arp Packets -----------------------------------------------------------------

-- | Arp packets, specialized to IP4 and Mac addresses.
data ArpPacket = ArpPacket { arpOper   :: {-# UNPACK #-} !ArpOper
                           , arpSHA    :: !Mac
                           , arpSPA    :: !IP4
                           , arpTHA    :: !Mac
                           , arpTPA    :: !IP4
                           } deriving (Eq,Show)

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


renderArpPacket :: ArpPacket -> L.ByteString
renderArpPacket pkt = runPutPacket 28 100 L.empty (putArpPacket pkt)

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
