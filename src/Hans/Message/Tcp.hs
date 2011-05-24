{-# LANGUAGE FlexibleInstances #-}

module Hans.Message.Tcp where

import Hans.Address.IP4 (IP4)
import Hans.Message.Ip4 (mkIP4PseudoHeader,IP4Protocol(..))
import Hans.Utils.Checksum (computePartialChecksum,computeChecksum,pokeChecksum)

import Control.Monad (when,unless,ap)
import Data.Bits ((.&.),setBit,testBit,shiftL,shiftR)
import Data.List (foldl',find)
import Data.Serialize
    (Get,Put,Putter,getWord16be,putWord16be,getWord32be,putWord32be,getWord8
    ,putWord8,putByteString,getBytes,remaining,label,isolate,skip,runPut)
import Data.Word (Word8,Word16,Word32)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S


-- Tcp Support Types -----------------------------------------------------------

tcpProtocol :: IP4Protocol
tcpProtocol  = IP4Protocol 0x6

newtype TcpPort = TcpPort
  { getPort :: Word16
  } deriving Show

putTcpPort :: Putter TcpPort
putTcpPort (TcpPort w16) = putWord16be w16

getTcpPort :: Get TcpPort
getTcpPort  = TcpPort `fmap` getWord16be


newtype TcpSeqNum = TcpSeqNum
  { getSeqNum :: Word32
  } deriving (Eq,Ord,Show)

putTcpSeqNum :: Putter TcpSeqNum
putTcpSeqNum (TcpSeqNum w32) = putWord32be w32

getTcpSeqNum :: Get TcpSeqNum
getTcpSeqNum  = TcpSeqNum `fmap` getWord32be


newtype TcpAckNum = TcpAckNum
  { getAckNum :: Word32
  } deriving (Eq,Ord,Show)

putTcpAckNum :: Putter TcpAckNum
putTcpAckNum (TcpAckNum w32) = putWord32be w32

getTcpAckNum :: Get TcpAckNum
getTcpAckNum  = TcpAckNum `fmap` getWord32be


-- Tcp Header ------------------------------------------------------------------

--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |          Source Port          |       Destination Port        |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                        Sequence Number                        |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                    Acknowledgment Number                      |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |  Data |       |C|E|U|A|P|R|S|F|                               |
--   | Offset| Res.  |W|C|R|C|S|S|Y|I|            Window             |
--   |       |       |R|E|G|K|H|T|N|N|                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |           Checksum            |         Urgent Pointer        |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                    Options                    |    Padding    |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                             data                              |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
data TcpHeader = TcpHeader
  { tcpSourcePort    :: !TcpPort
  , tcpDestPort      :: !TcpPort
  , tcpSeqNum        :: !TcpSeqNum
  , tcpAckNum        :: !TcpAckNum
  , tcpCwr           :: !Bool
  , tcpEce           :: !Bool
  , tcpUrg           :: !Bool
  , tcpAck           :: !Bool
  , tcpPsh           :: !Bool
  , tcpRst           :: !Bool
  , tcpSyn           :: !Bool
  , tcpFin           :: !Bool
  , tcpWindow        :: !Word16
  , tcpChecksum      :: !Word16
  , tcpUrgentPointer :: !Word16
  , tcpOptions       :: [TcpOption]
  } deriving Show

instance HasTcpOptions TcpHeader where
  findTcpOption tag hdr = findTcpOption tag (tcpOptions hdr)
  setTcpOption  opt hdr = hdr { tcpOptions = setTcpOption opt (tcpOptions hdr) }

emptyTcpHeader :: TcpHeader
emptyTcpHeader  = TcpHeader
  { tcpSourcePort    = TcpPort 0
  , tcpDestPort      = TcpPort 0
  , tcpSeqNum        = TcpSeqNum 0
  , tcpAckNum        = TcpAckNum 0
  , tcpCwr           = False
  , tcpEce           = False
  , tcpUrg           = False
  , tcpAck           = False
  , tcpPsh           = False
  , tcpRst           = False
  , tcpSyn           = False
  , tcpFin           = False
  , tcpWindow        = 0
  , tcpChecksum      = 0
  , tcpUrgentPointer = 0
  , tcpOptions       = []
  }

-- | The length of the fixed part of the TcpHeader, in 4-byte octets.
tcpFixedHeaderLength :: Int
tcpFixedHeaderLength  = 5

-- | Calculate the length of a TcpHeader, in 4-byte octets.
tcpHeaderLength :: TcpHeader -> Int
tcpHeaderLength hdr =
  tcpFixedHeaderLength + tcpOptionsLength (tcpOptions hdr)

-- | Render a TcpHeader.  The checksum value is never rendered, as it is
-- expected to be calculated and poked in afterwords.
putTcpHeader :: Putter TcpHeader
putTcpHeader hdr = do
  putTcpPort (tcpSourcePort hdr)
  putTcpPort (tcpDestPort hdr)
  putTcpSeqNum (tcpSeqNum hdr)
  putTcpAckNum (tcpAckNum hdr)
  putWord8 (fromIntegral (tcpHeaderLength hdr) `shiftL` 4)
  putTcpControl hdr
  putWord16be (tcpWindow hdr)
  putWord16be 0
  putWord16be (tcpUrgentPointer hdr)
  putTcpOptions (tcpOptions hdr)

-- | Parse out a TcpHeader, and its length.  The resulting length is in bytes,
-- and is derived from the data offset.
getTcpHeader :: Get (TcpHeader,Int)
getTcpHeader  = do
  src    <- getTcpPort
  dst    <- getTcpPort
  seqNum <- getTcpSeqNum
  ackNum <- getTcpAckNum
  b      <- getWord8
  let len = fromIntegral ((b `shiftR` 4) .&. 0xf)
  cont   <- getWord8
  win    <- getWord16be
  cs     <- getWord16be
  urgent <- getWord16be
  let optsLen = len - tcpFixedHeaderLength
  opts   <- getTcpOptions optsLen
  let hdr = setTcpControl cont emptyTcpHeader
        { tcpSourcePort    = src
        , tcpDestPort      = dst
        , tcpSeqNum        = seqNum
        , tcpAckNum        = ackNum
        , tcpWindow        = win
        , tcpChecksum      = cs
        , tcpUrgentPointer = urgent
        , tcpOptions       = opts
        }
  return (hdr,len * 4)

-- | Render out the @Word8@ that contains the Control field of the TcpHeader.
putTcpControl :: Putter TcpHeader
putTcpControl c =
  putWord8 $ putBit 7 tcpCwr
           $ putBit 6 tcpEce
           $ putBit 5 tcpUrg
           $ putBit 4 tcpAck
           $ putBit 3 tcpPsh
           $ putBit 2 tcpRst
           $ putBit 1 tcpSyn
           $ putBit 0 tcpFin
             0
  where
  putBit n prj w | prj c     = setBit w n
                 | otherwise = w

-- | Parse out the control flags from the octet that contains them.
setTcpControl :: Word8 -> TcpHeader -> TcpHeader
setTcpControl w hdr = hdr
  { tcpCwr = testBit w 7
  , tcpEce = testBit w 6
  , tcpUrg = testBit w 5
  , tcpAck = testBit w 4
  , tcpPsh = testBit w 3
  , tcpRst = testBit w 2
  , tcpSyn = testBit w 1
  , tcpFin = testBit w 0
  }


-- Tcp Options -----------------------------------------------------------------

class HasTcpOptions a where
  findTcpOption :: TcpOptionTag -> a -> Maybe TcpOption
  setTcpOption  :: TcpOption    -> a -> a

data TcpOptionTag
  = OptTagEndOfOptions
  | OptTagNoOption
  | OptTagMaxSegmentSize
  | OptTagWindowScaling
  | OptTagTimestamp
  | OptTagUnknown !Word8
    deriving (Eq,Show)

getTcpOptionTag :: Get TcpOptionTag
getTcpOptionTag  = do
  ty <- getWord8
  return $! case ty of
    0 -> OptTagEndOfOptions
    1 -> OptTagNoOption
    2 -> OptTagMaxSegmentSize
    3 -> OptTagWindowScaling
    8 -> OptTagTimestamp
    _ -> OptTagUnknown ty

putTcpOptionTag :: Putter TcpOptionTag
putTcpOptionTag tag =
  putWord8 $ case tag of
    OptTagEndOfOptions   -> 0
    OptTagNoOption       -> 1
    OptTagMaxSegmentSize -> 2
    OptTagWindowScaling  -> 3
    OptTagTimestamp      -> 8
    OptTagUnknown ty     -> ty

instance HasTcpOptions [TcpOption] where
  findTcpOption tag = find p
    where
    p opt = tag == tcpOptionTag opt

  setTcpOption opt = loop
    where
    tag           = tcpOptionTag opt
    loop []       = [opt]
    loop (o:opts)
      | tcpOptionTag o == tag = opt : opts
      | otherwise             = o : loop opts


data TcpOption
  = OptEndOfOptions
  | OptNoOption
  | OptMaxSegmentSize !Word16
  | OptWindowScaling !Word8
  | OptTimestamp !Word32 !Word32
  | OptUnknown !Word8 !Word8 !S.ByteString
    deriving Show

tcpOptionTag :: TcpOption -> TcpOptionTag
tcpOptionTag opt = case opt of
  OptEndOfOptions{}   -> OptTagEndOfOptions
  OptNoOption{}       -> OptTagNoOption
  OptMaxSegmentSize{} -> OptTagMaxSegmentSize
  OptWindowScaling{}  -> OptTagWindowScaling
  OptTimestamp{}      -> OptTagTimestamp
  OptUnknown ty _ _   -> OptTagUnknown ty

-- | Get the length of a TcpOptions, in 4-byte words.  This rounds up to the
-- nearest 4-byte word.
tcpOptionsLength :: [TcpOption] -> Int
tcpOptionsLength opts
  | left == 0 = len
  | otherwise = len + 1
  where
  (len,left)   = foldl' step 0 opts `quotRem` 4
  step acc opt = tcpOptionLength opt + acc

tcpOptionLength :: TcpOption -> Int
tcpOptionLength OptEndOfOptions{}    = 1
tcpOptionLength OptNoOption{}        = 1
tcpOptionLength OptMaxSegmentSize{}  = 4
tcpOptionLength OptWindowScaling{}   = 3
tcpOptionLength OptTimestamp{}       = 10
tcpOptionLength (OptUnknown _ len _) = fromIntegral len


-- | Render out the tcp options, and pad with zeros if they don't fall on a
-- 4-byte boundary.
putTcpOptions :: Putter [TcpOption]
putTcpOptions opts = do
  let len     = tcpOptionsLength opts
      left    = len `rem` 4
      padding
        | left == 0 = 0
        | otherwise = 4 - left
  mapM_ putTcpOption opts
  when (padding > 0) (putByteString (S.replicate padding 0))

putTcpOption :: Putter TcpOption
putTcpOption opt =
  case opt of
    OptEndOfOptions       -> putWord8 0
    OptNoOption           -> putWord8 1
    OptMaxSegmentSize mss -> putMaxSegmentSize mss
    OptWindowScaling w    -> putWindowScaling w
    OptTimestamp v r      -> putTimestamp v r
    OptUnknown ty len bs  -> putUnknown ty len bs

-- | Parse in known tcp options.
getTcpOptions :: Int -> Get [TcpOption]
getTcpOptions len = label ("Tcp Options (" ++ show len ++ ")")
                  $ isolate (len * 4) loop
  where
  loop = do
    left <- remaining
    if left <= 0 then return [] else body

  body = do
    opt  <- getTcpOption
    case opt of
      OptNoOption -> loop

      OptEndOfOptions -> do
        skip =<< remaining
        return []

      _ -> do
        rest <- loop
        return (opt:rest)

getTcpOption :: Get TcpOption
getTcpOption  = do
  tag <- getTcpOptionTag
  case tag of
    OptTagEndOfOptions   -> return OptEndOfOptions
    OptTagNoOption       -> return OptNoOption
    OptTagMaxSegmentSize -> getMaxSegmentSize
    OptTagWindowScaling  -> getWindowScaling
    OptTagTimestamp      -> getTimestamp
    OptTagUnknown ty     -> getUnknown ty

getMaxSegmentSize :: Get TcpOption
getMaxSegmentSize  = label "Max Segment Size" $ isolate 3 $ do
  len <- getWord8
  unless (len == 4) (fail ("Unexpected length: " ++ show len))
  OptMaxSegmentSize `fmap` getWord16be

putMaxSegmentSize :: Putter Word16
putMaxSegmentSize w16 = do
  putWord8 4
  putWord16be w16

getWindowScaling :: Get TcpOption
getWindowScaling  = label "Window Scaling" $ isolate 2 $ do
  len <- getWord8
  unless (len == 3) (fail ("Unexpected length: " ++ show len))
  OptWindowScaling `fmap` getWord8

putWindowScaling :: Putter Word8
putWindowScaling w = do
  putWord8 3
  putWord8 w

getTimestamp :: Get TcpOption
getTimestamp  = label "Timestamp" $ isolate 9 $ do
  len <- getWord8
  unless (len == 10) (fail ("Unexpected length: " ++ show len))
  OptTimestamp `fmap` getWord32be `ap` getWord32be

putTimestamp :: Word32 -> Word32 -> Put
putTimestamp v r = do
  putWord8 8
  putWord8 10
  putWord32be v
  putWord32be r

getUnknown :: Word8 -> Get TcpOption
getUnknown ty = do
  len  <- getWord8
  body <- isolate (fromIntegral len - 2) (getBytes =<< remaining)
  return (OptUnknown ty len body)

putUnknown :: Word8 -> Word8 -> S.ByteString -> Put
putUnknown ty len body = do
  putWord8 ty
  putWord8 len
  putByteString body


-- Tcp Packet ------------------------------------------------------------------

data TcpPacket = TcpPacket
  { tcpHeader :: !TcpHeader
  , tcpBody   :: !S.ByteString
  } deriving Show

-- | Parse a TcpPacket.
getTcpPacket :: Get TcpPacket
getTcpPacket  = do
  pktLen       <- remaining
  (hdr,hdrLen) <- getTcpHeader
  body         <- getBytes (pktLen - hdrLen)
  return (TcpPacket hdr body)

-- | Render out a TcpPacket, without calculating its checksum.
putTcpPacket :: Putter TcpPacket
putTcpPacket (TcpPacket hdr body) = do
  putTcpHeader hdr
  putByteString body

-- | Calculate the checksum of a TcpHeader, and its body.
renderWithTcpChecksumIP4 :: IP4 -> IP4 -> TcpPacket -> S.ByteString
renderWithTcpChecksumIP4 src dst pkt@(TcpPacket _ body) = hdrbs `S.append` body
  where
  (hdrbs,_) = computeTcpChecksumIP4 src dst pkt

-- | Calculate the checksum of a tcp packet, and return its rendered header.
computeTcpChecksumIP4 :: IP4 -> IP4 -> TcpPacket -> (S.ByteString,Word16)
computeTcpChecksumIP4 src dst (TcpPacket hdr body) =
  -- this is safe, as the header bytestring that gets modified is modified at
  -- its creation time.
  (cs `seq` unsafePerformIO (pokeChecksum cs hdrbs 16), cs)
  where
  hdrbs = runPut (putTcpHeader hdr { tcpChecksum = 0 })
  phcs  = computePartialChecksum 0
        $ mkIP4PseudoHeader src dst tcpProtocol
        $ S.length hdrbs + S.length body
  hdrcs = computePartialChecksum phcs hdrbs
  cs    = computeChecksum hdrcs body

-- | Re-create the checksum, minimizing duplication of the original, rendered
-- TCP packet.
recreateTcpChecksumIP4 :: IP4 -> IP4 -> S.ByteString -> Word16
recreateTcpChecksumIP4 src dst bytes = computeChecksum hdrcs rest
  where
  phcs         = computePartialChecksum 0
               $ mkIP4PseudoHeader src dst tcpProtocol
               $ S.length bytes
  (hdrbs,rest) = S.splitAt 18 bytes
  hdrbs'       = unsafePerformIO (pokeChecksum 0 (S.copy hdrbs) 16)
  hdrcs        = computePartialChecksum phcs hdrbs'

