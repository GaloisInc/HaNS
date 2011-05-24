{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Ip4 where

import Hans.Address.IP4 (IP4)
import Hans.Utils
import Hans.Utils.Checksum

import Control.Monad (unless)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get,getWord8,getWord16be,getByteString,isolate,label)
import Data.Serialize.Put (runPut,runPutM,putWord8,putWord16be,putByteString)
import Data.Bits (Bits((.&.),(.|.),testBit,setBit,shiftR,shiftL,bit))
import Data.Word (Word8,Word16)
import qualified Data.ByteString as S


-- IP4 Pseudo Header -----------------------------------------------------------

-- 0      7 8     15 16    23 24    31 
-- +--------+--------+--------+--------+
-- |          source address           |
-- +--------+--------+--------+--------+
-- |        destination address        |
-- +--------+--------+--------+--------+
-- |  zero  |protocol|     length      |
-- +--------+--------+--------+--------+
mkIP4PseudoHeader :: IP4 -> IP4 -> IP4Protocol -> MkPseudoHeader
mkIP4PseudoHeader src dst prot len = runPut $ do
  put src
  put dst
  putWord8 0 >> put prot >> putWord16be (fromIntegral len)


-- IP4 Packets -----------------------------------------------------------------

newtype Ident = Ident { getIdent :: Word16 }
  deriving (Eq,Ord,Num,Show,Serialize,Integral,Real,Enum)

newtype IP4Protocol = IP4Protocol { getIP4Protocol :: Word8 }
  deriving (Eq,Ord,Num,Show,Serialize)

data IP4Packet = IP4Packet
  { ip4Header  :: !IP4Header
  , ip4Payload :: S.ByteString
  } deriving Show

data IP4Header = IP4Header
  { ip4Version        :: !Word8
  , ip4TypeOfService  :: !Word8
  , ip4Ident          :: !Ident
  , ip4MayFragment    :: Bool
  , ip4MoreFragments  :: Bool
  , ip4FragmentOffset :: !Word16
  , ip4TimeToLive     :: !Word8
  , ip4Protocol       :: !IP4Protocol
  , ip4Checksum       :: !Word16
  , ip4SourceAddr     :: !IP4
  , ip4DestAddr       :: !IP4
  , ip4Options        :: [IP4Option]
  } deriving Show

emptyIP4Header :: IP4Protocol -> IP4 -> IP4 -> IP4Header
emptyIP4Header prot src dst = IP4Header
  { ip4Version        = 4
  , ip4TypeOfService  = 0
  , ip4Ident          = 0
  , ip4MayFragment    = False
  , ip4MoreFragments  = False
  , ip4FragmentOffset = 0
  , ip4TimeToLive     = 127
  , ip4Protocol       = prot
  , ip4Checksum       = 0
  , ip4SourceAddr     = src
  , ip4DestAddr       = dst
  , ip4Options        = []
  }


noMoreFragments :: IP4Header -> IP4Header
noMoreFragments hdr = hdr { ip4MoreFragments = False }

moreFragments :: IP4Header -> IP4Header
moreFragments hdr = hdr { ip4MoreFragments = True }

addOffset :: Word16 -> IP4Header -> IP4Header
addOffset off hdr = hdr { ip4FragmentOffset = ip4FragmentOffset hdr + off }

setIdent :: Ident -> IP4Header -> IP4Header
setIdent i hdr = hdr { ip4Ident = i }


-- | Calculate the size of an IP4 packet
ip4PacketSize :: IP4Packet -> Int
ip4PacketSize (IP4Packet hdr bs) =
  ip4HeaderSize hdr + fromIntegral (S.length bs)

-- | Calculate the size of an IP4 header
ip4HeaderSize :: IP4Header -> Int
ip4HeaderSize hdr = 20 + sum (map ip4OptionSize (ip4Options hdr))


-- | Fragment a single IP packet into one or more, given an MTU to fit into.
splitPacket :: Int -> IP4Packet -> [IP4Packet]
splitPacket mtu pkt
  | ip4PacketSize pkt > mtu = fragmentPacket mtu' pkt
  | otherwise               = [pkt]
  where
  mtu' = fromIntegral (mtu - ip4HeaderSize (ip4Header pkt))


-- | Given a fragment size and a packet, fragment the packet into multiple
-- smaller ones.
fragmentPacket :: Int -> IP4Packet -> [IP4Packet]
fragmentPacket mtu pkt@(IP4Packet hdr bs)
  | payloadLen <= mtu = [pkt { ip4Header = noMoreFragments hdr }]
  | otherwise         = frag : fragmentPacket mtu pkt'
  where
  payloadLen = S.length bs
  (as,rest)  = S.splitAt mtu bs
  alen       = fromIntegral (S.length as)
  pkt'       = pkt { ip4Header = hdr', ip4Payload = rest }
  hdr'       = addOffset alen hdr
  frag       = pkt { ip4Header = moreFragments hdr, ip4Payload = as }


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
parseIP4Packet :: Get (IP4Header, Int, Int)
parseIP4Packet = do
  b0    <- getWord8
  let ver = b0 `shiftR` 4
  let ihl = fromIntegral ((b0 .&. 0xf) * 4)
  label "IP4 Header" $ isolate (ihl - 1) $ do
    tos   <- getWord8
    len   <- getWord16be
    ident <- get
    b1    <- getWord16be
    let flags = b1 `shiftR` 13
    let off   = b1 .&. 0x1fff
    ttl     <- getWord8
    prot    <- get
    cs      <- getWord16be
    source  <- get
    dest    <- get
    let optlen = ihl - 20
    opts <- label "IP4 Options"
          $ isolate optlen 
          $ getOptions 
          $ fromIntegral optlen
    let hdr = IP4Header
          { ip4Version        = ver
          , ip4TypeOfService  = tos
          , ip4Ident          = ident
          , ip4MayFragment    = flags `testBit` 1
          , ip4MoreFragments  = flags `testBit` 0
          , ip4FragmentOffset = off * 8
          , ip4TimeToLive     = ttl
          , ip4Protocol       = prot
          , ip4Checksum       = cs
          , ip4SourceAddr     = source
          , ip4DestAddr       = dest
          , ip4Options        = opts
          }
    return (hdr, fromIntegral ihl, fromIntegral len)


-- | The final step to render an IP header and its payload out as a bytestring.
renderIP4Packet :: IP4Packet -> IO Packet
renderIP4Packet (IP4Packet hdr pkt) = do
  let (len,bs) = runPutM $ do
        let (optbs,optlen) = renderOptions (ip4Options hdr)
        let ihl            = 20 + optlen
        putWord8    (ip4Version hdr `shiftL` 4 .|. (ihl `div` 4))
        putWord8    (ip4TypeOfService hdr)
        putWord16be (fromIntegral (S.length pkt) + fromIntegral ihl)

        put (ip4Ident hdr)
        let frag | ip4MayFragment hdr = (`setBit` 1)
                 | otherwise          = id
        let morefrags | ip4MoreFragments hdr = (`setBit` 0)
                      | otherwise            = id
        let flags = frag (morefrags 0)
        let off   = ip4FragmentOffset hdr `div` 8
        putWord16be (flags `shiftL` 13 .|. off .&. 0x1fff)

        putWord8    (ip4TimeToLive hdr)
        put         (ip4Protocol hdr)
        putWord16be 0 -- (ip4Checksum hdr)

        put (ip4SourceAddr hdr)

        put (ip4DestAddr hdr)

        putByteString optbs

        putByteString pkt

        return ihl
  let cs = computeChecksum 0 (S.take (fromIntegral len) bs)
  pokeChecksum cs bs 10


-- IP4 Options -----------------------------------------------------------------

renderOptions :: [IP4Option] -> (S.ByteString,Word8)
renderOptions opts = case optlen `mod` 4 of
  0 -> (optbs,fromIntegral optlen)
  -- pad with no-ops
  n -> (optbs `S.append` S.replicate n 0x1, fromIntegral (optlen + n))
  where
  optbs  = runPut (mapM_ put opts)
  optlen = S.length optbs


getOptions :: Int -> Get [IP4Option]
getOptions len
  | len <= 0  = return []
  | otherwise = do
    o    <- get
    rest <- getOptions (len - ip4OptionSize o)
    return $! (o : rest)


data IP4Option = IP4Option
  { ip4OptionCopied :: !Bool
  , ip4OptionClass  :: !Word8
  , ip4OptionNum    :: !Word8
  , ip4OptionData   :: S.ByteString
  } deriving Show


ip4OptionSize :: IP4Option -> Int
ip4OptionSize opt = case ip4OptionNum opt of
  0 -> 1
  1 -> 1
  _ -> 2 + fromIntegral (S.length (ip4OptionData opt))


instance Serialize IP4Option where
  get = do
    b <- getWord8
    let optCopied = testBit b 7
    let optClass  = (b `shiftR` 5) .&. 0x3
    let optNum    = b .&. 0x1f
    bs <- case optNum of
      0 -> return S.empty
      1 -> return S.empty
      _ -> do
        len <- getWord8
        unless (len >= 2) (fail "Option length parameter is to small")
        getByteString (fromIntegral (len - 2))
    return $! IP4Option
      { ip4OptionCopied = optCopied
      , ip4OptionClass  = optClass
      , ip4OptionNum    = optNum
      , ip4OptionData   = bs
      }
  put opt = do
    let copied | ip4OptionCopied opt = bit 7
               | otherwise           = 0
    putWord8 (copied .|. ((ip4OptionClass opt .&. 0x3) `shiftL` 5)
                 .|.  ip4OptionNum    opt .&. 0x1f)
    case ip4OptionNum opt of
      0 -> return ()
      1 -> return ()
      _ -> do
        putWord8 (fromIntegral (S.length (ip4OptionData opt)))
        putByteString (ip4OptionData opt)
