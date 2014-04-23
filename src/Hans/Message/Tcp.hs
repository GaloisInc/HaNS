{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Tcp where

import Hans.Address.IP4 (IP4)
import Hans.Message.Ip4 (mkIP4PseudoHeader,IP4Protocol(..))
import Hans.Utils (chunk)
import Hans.Utils.Checksum

import Control.Monad (unless,ap,replicateM_,replicateM)
import Data.Bits ((.&.),setBit,testBit,shiftL,shiftR)
import Data.List (find)
import Data.Monoid (Monoid(..))
import Data.Serialize
    (Get,Put,Putter,getWord16be,putWord16be,getWord32be,putWord32be,getWord8
    ,putWord8,putByteString,getBytes,remaining,label,isolate,skip,runGet,runPut
    ,putLazyByteString)
import Data.Word (Word8,Word16,Word32)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S
import qualified Data.Foldable        as F


-- Tcp Support Types -----------------------------------------------------------

tcpProtocol :: IP4Protocol
tcpProtocol  = IP4Protocol 0x6

newtype TcpPort = TcpPort
  { getPort :: Word16
  } deriving (Eq,Ord,Read,Show,Num,Enum,Bounded)

putTcpPort :: Putter TcpPort
putTcpPort (TcpPort w16) = putWord16be w16

getTcpPort :: Get TcpPort
getTcpPort  = TcpPort `fmap` getWord16be


newtype TcpSeqNum = TcpSeqNum
  { getSeqNum :: Word32
  } deriving (Eq,Ord,Show,Num,Bounded,Enum,Real,Integral)

instance Monoid TcpSeqNum where
  mempty  = 0
  mappend = (+)

putTcpSeqNum :: Putter TcpSeqNum
putTcpSeqNum (TcpSeqNum w32) = putWord32be w32

getTcpSeqNum :: Get TcpSeqNum
getTcpSeqNum  = TcpSeqNum `fmap` getWord32be


-- | An alias to TcpSeqNum, as these two are used in the same role.
type TcpAckNum = TcpSeqNum

putTcpAckNum :: Putter TcpAckNum
putTcpAckNum  = putTcpSeqNum

getTcpAckNum :: Get TcpAckNum
getTcpAckNum  = getTcpSeqNum


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
  } deriving (Eq,Show)

instance HasTcpOptions TcpHeader where
  findTcpOption tag hdr = findTcpOption tag (tcpOptions hdr)
  setTcpOption  opt hdr = hdr { tcpOptions = setTcpOption opt (tcpOptions hdr) }

emptyTcpHeader :: TcpHeader
emptyTcpHeader  = TcpHeader
  { tcpSourcePort    = TcpPort 0
  , tcpDestPort      = TcpPort 0
  , tcpSeqNum        = 0
  , tcpAckNum        = 0
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

-- | Render a TcpHeader.  The checksum value is never rendered, as it is
-- expected to be calculated and poked in afterwords.
putTcpHeader :: Putter TcpHeader
putTcpHeader hdr = do
  putTcpPort (tcpSourcePort hdr)
  putTcpPort (tcpDestPort hdr)
  putTcpSeqNum (tcpSeqNum hdr)
  putTcpAckNum (tcpAckNum hdr)
  let (optLen,padding) = tcpOptionsLength (tcpOptions hdr)
  putWord8 (fromIntegral ((tcpFixedHeaderLength + optLen) `shiftL` 4))
  putTcpControl hdr
  putWord16be (tcpWindow hdr)
  putWord16be 0
  putWord16be (tcpUrgentPointer hdr)
  mapM_ putTcpOption (tcpOptions hdr)
  replicateM_ padding (putTcpOptionTag OptTagEndOfOptions)

-- | Parse out a TcpHeader, and its length.  The resulting length is in bytes,
-- and is derived from the data offset.
getTcpHeader :: Get (TcpHeader,Int)
getTcpHeader  = label "TcpHeader" $ do
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
  opts   <- label "options" (isolate (optsLen `shiftL` 2) getTcpOptions)
  let hdr = setTcpControl cont emptyTcpHeader
        { tcpSourcePort    = src
        , tcpDestPort      = dst
        , tcpSeqNum        = seqNum
        , tcpAckNum        = ackNum
        , tcpWindow        = win
        , tcpChecksum      = cs
        , tcpUrgentPointer = urgent
        , tcpOptions       = filter (/= OptEndOfOptions) opts
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

setTcpOptions :: HasTcpOptions a => [TcpOption] -> a -> a
setTcpOptions opts a = foldr setTcpOption a opts

data TcpOptionTag
  = OptTagEndOfOptions
  | OptTagNoOption
  | OptTagMaxSegmentSize
  | OptTagWindowScaling
  | OptTagSackPermitted
  | OptTagSack
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
    4 -> OptTagSackPermitted
    5 -> OptTagSack
    8 -> OptTagTimestamp
    _ -> OptTagUnknown ty

putTcpOptionTag :: Putter TcpOptionTag
putTcpOptionTag tag =
  putWord8 $ case tag of
    OptTagEndOfOptions   -> 0
    OptTagNoOption       -> 1
    OptTagMaxSegmentSize -> 2
    OptTagWindowScaling  -> 3
    OptTagSackPermitted  -> 4
    OptTagSack           -> 5
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
  | OptSackPermitted
  | OptSack [SackBlock]
  | OptTimestamp !Word32 !Word32
  | OptUnknown !Word8 !Word8 !S.ByteString
    deriving (Show,Eq)

data SackBlock = SackBlock
  { sbLeft  :: !TcpSeqNum
  , sbRight :: !TcpSeqNum
  } deriving (Show,Eq)

tcpOptionTag :: TcpOption -> TcpOptionTag
tcpOptionTag opt = case opt of
  OptEndOfOptions{}   -> OptTagEndOfOptions
  OptNoOption{}       -> OptTagNoOption
  OptMaxSegmentSize{} -> OptTagMaxSegmentSize
  OptSackPermitted{}  -> OptTagSackPermitted
  OptSack{}           -> OptTagSack
  OptWindowScaling{}  -> OptTagWindowScaling
  OptTimestamp{}      -> OptTagTimestamp
  OptUnknown ty _ _   -> OptTagUnknown ty

-- | Get the rendered length of a list of TcpOptions, in 4-byte words, and the
-- number of padding bytes required.  This rounds up to the nearest 4-byte word.
tcpOptionsLength :: [TcpOption] -> (Int,Int)
tcpOptionsLength opts
  | left == 0 = (len,0)
  | otherwise = (len + 1,4 - left)
  where
  (len,left) = F.sum (fmap tcpOptionLength opts) `quotRem` 4

tcpOptionLength :: TcpOption -> Int
tcpOptionLength opt = case opt of
  OptEndOfOptions{}   -> 1
  OptNoOption{}       -> 1
  OptMaxSegmentSize{} -> 4
  OptWindowScaling{}  -> 3
  OptSackPermitted{}  -> 2
  OptSack bs          -> sackLength bs
  OptTimestamp{}      -> 10
  OptUnknown _ len _  -> fromIntegral len


putTcpOption :: Putter TcpOption
putTcpOption opt = do
  putTcpOptionTag (tcpOptionTag opt)
  case opt of
    OptEndOfOptions       -> return ()
    OptNoOption           -> return ()
    OptMaxSegmentSize mss -> putMaxSegmentSize mss
    OptWindowScaling w    -> putWindowScaling w
    OptSackPermitted      -> putSackPermitted
    OptSack bs            -> putSack bs
    OptTimestamp v r      -> putTimestamp v r
    OptUnknown _ len bs   -> putUnknown len bs

-- | Parse in known tcp options.
getTcpOptions :: Get [TcpOption]
getTcpOptions  = label "Tcp Options" loop
  where
  loop = do
    left <- remaining
    if left > 0 then body else return []

  body = do
    opt <- getTcpOption
    case opt of

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
    OptTagSackPermitted  -> getSackPermitted
    OptTagSack           -> getSack
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

getSackPermitted :: Get TcpOption
getSackPermitted  = label "Sack Permitted" $ isolate 1 $ do
  len <- getWord8
  unless (len == 2) (fail ("Unexpected length: " ++ show len))
  return OptSackPermitted

putSackPermitted :: Put
putSackPermitted  = do
  putWord8 2

getSack :: Get TcpOption
getSack  = label "Sack" $ do
  len <- getWord8
  let edgeLen = fromIntegral len - 2
  OptSack `fmap` isolate edgeLen (replicateM (edgeLen `shiftR` 3) getSackBlock)

putSack :: Putter [SackBlock]
putSack bs = do
  putWord8 (fromIntegral (sackLength bs))
  mapM_ putSackBlock bs

getSackBlock :: Get SackBlock
getSackBlock  = do
  l <- getTcpSeqNum
  r <- getTcpSeqNum
  return $! SackBlock
    { sbLeft  = l
    , sbRight = r
    }

putSackBlock :: Putter SackBlock
putSackBlock sb = do
  putTcpSeqNum (sbLeft sb)
  putTcpSeqNum (sbRight sb)

sackLength :: [SackBlock] -> Int
sackLength bs = length bs * 8 + 2

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
  putWord8 10
  putWord32be v
  putWord32be r

getUnknown :: Word8 -> Get TcpOption
getUnknown ty = do
  len  <- getWord8
  body <- isolate (fromIntegral len - 2) (getBytes =<< remaining)
  return (OptUnknown ty len body)

putUnknown :: Word8 -> S.ByteString -> Put
putUnknown len body = do
  putWord8 len
  putByteString body


-- Tcp Packet ------------------------------------------------------------------

{-# INLINE parseTcpPacket #-}
parseTcpPacket :: S.ByteString -> Either String (TcpHeader,S.ByteString)
parseTcpPacket bytes = runGet getTcpPacket bytes

-- | Parse a TcpPacket.
getTcpPacket :: Get (TcpHeader,S.ByteString)
getTcpPacket  = do
  pktLen       <- remaining
  (hdr,hdrLen) <- getTcpHeader
  body         <- getBytes (pktLen - hdrLen)
  return (hdr,body)

-- | Render out a TcpPacket, without calculating its checksum.
putTcpPacket :: TcpHeader -> L.ByteString -> Put
putTcpPacket hdr body = do
  putTcpHeader hdr
  putLazyByteString body

-- | Calculate the checksum of a TcpHeader, and its body.
renderWithTcpChecksumIP4 :: IP4 -> IP4 -> TcpHeader -> L.ByteString
                         -> L.ByteString
renderWithTcpChecksumIP4 src dst hdr body = chunk hdrbs `L.append` body
  where
  (hdrbs,_) = computeTcpChecksumIP4 src dst hdr body

-- | Calculate the checksum of a tcp packet, and return its rendered header.
computeTcpChecksumIP4 :: IP4 -> IP4 -> TcpHeader -> L.ByteString
                      -> (S.ByteString,Word16)
computeTcpChecksumIP4 src dst hdr body =
  -- this is safe, as the header bytestring that gets modified is modified at
  -- its creation time.
  (cs `seq` unsafePerformIO (pokeChecksum cs hdrbs 16), cs)
  where
  phcs  = computePartialChecksum emptyPartialChecksum
        $ mkIP4PseudoHeader src dst tcpProtocol
        $ S.length hdrbs + fromIntegral (L.length body)
  hdrbs = runPut (putTcpHeader hdr { tcpChecksum = 0 })
  hdrcs = computePartialChecksum phcs hdrbs
  cs    = finalizeChecksum (computePartialChecksumLazy hdrcs body)

-- | Re-create the checksum, minimizing duplication of the original, rendered
-- TCP packet.
validateTcpChecksumIP4 :: IP4 -> IP4 -> S.ByteString -> Bool
validateTcpChecksumIP4 src dst bytes =
  finalizeChecksum (computePartialChecksum phcs bytes) == 0
  where
  phcs = computePartialChecksum emptyPartialChecksum
       $ mkIP4PseudoHeader src dst tcpProtocol
       $ S.length bytes

