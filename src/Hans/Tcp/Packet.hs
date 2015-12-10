{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}

module Hans.Tcp.Packet (

    -- * Header
    TcpHeader(..),
    TcpPort,
    TcpSeqNum,
    emptyTcpHeader,

    -- ** Header Flags
    tcpNs, tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpSyn,
    tcpFin,

    -- ** Serialization
    getTcpHeader, putTcpHeader,

    -- ** Options
    HasTcpOptions(..),
    findTcpOption,
    setTcpOption,
    setTcpOptions,
    TcpOption(..),
    TcpOptionTag(..), tcpOptionTag,
    SackBlock(..),

    -- * Segment Operations
    tcpSegLen,
    tcpSegLastSeqNum,
    tcpSegNextAckNum,
  ) where

import Hans.Checksum (finalizeChecksum,extendChecksum)
import Hans.Lens
import Hans.Serialize (runPutPacket)

import           Control.Monad (replicateM,replicateM_,unless)
import           Data.Bits ((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import           Data.List (find)
import           Data.Serialize.Get
                     (Get,getWord8,getWord16be,getWord32be,label,isolate
                     ,getBytes,remaining,skip)
import           Data.Serialize.Put
                     (Putter,Put,putWord8,putWord16be,putWord32be,putByteString)
import           Data.Word (Word8,Word16,Word32)


-- Tcp Support Types -----------------------------------------------------------

type TcpPort = Word16

putTcpPort :: Putter TcpPort
putTcpPort  = putWord16be

getTcpPort :: Get TcpPort
getTcpPort  = getWord16be


type TcpSeqNum = Word32

putTcpSeqNum :: Putter TcpSeqNum
putTcpSeqNum  = putWord32be

getTcpSeqNum :: Get TcpSeqNum
getTcpSeqNum  = getWord32be

-- | Checks that the second sequence number is withing the range defined by the
-- other two: a <= b <= c.
seqNumInRange :: TcpSeqNum -> TcpSeqNum -> TcpSeqNum -> Bool
seqNumInRange lo x hi

  | lo < hi   = lo <= x && x <= hi

    -- the bounds have wrapped
  | otherwise = lo <= x || x <= hi



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
--   |  Data |     |N|C|E|U|A|P|R|S|F|                               |
--   | Offset| Res.|S|W|C|R|C|S|S|Y|I|            Window             |
--   |       |     | |R|E|G|K|H|T|N|N|                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |           Checksum            |         Urgent Pointer        |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                    Options                    |    Padding    |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                             data                              |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
data TcpHeader = TcpHeader { tcpSourcePort    :: !TcpPort
                           , tcpDestPort      :: !TcpPort
                           , tcpSeqNum        :: !TcpSeqNum
                           , tcpAckNum        :: !TcpAckNum
                           , tcpFlags_        :: !Word16
                           , tcpWindow        :: !Word16
                           , tcpChecksum      :: !Word16
                           , tcpUrgentPointer :: !Word16
                           , tcpOptions_      :: [TcpOption]
                           } deriving (Eq,Show)

emptyTcpHeader :: TcpHeader
emptyTcpHeader  = TcpHeader { tcpSourcePort    = 0
                            , tcpDestPort      = 0
                            , tcpSeqNum        = 0
                            , tcpAckNum        = 0
                            , tcpFlags_        = 0
                            , tcpWindow        = 0
                            , tcpChecksum      = 0
                            , tcpUrgentPointer = 0
                            , tcpOptions_      = []
                            }

tcpFlags :: Lens' TcpHeader Word16
tcpFlags f hdr =
  fmap (\flags' -> hdr { tcpFlags_ = flags' }) (f (tcpFlags_ hdr))
{-# INLINE tcpFlags #-}

tcpNs, tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpSyn,
  tcpFin :: Lens' TcpHeader Bool
tcpNs  = tcpFlags . bit 8
tcpCwr = tcpFlags . bit 7
tcpEce = tcpFlags . bit 6
tcpUrg = tcpFlags . bit 5
tcpAck = tcpFlags . bit 4
tcpPsh = tcpFlags . bit 3
tcpRst = tcpFlags . bit 2
tcpSyn = tcpFlags . bit 1
tcpFin = tcpFlags . bit 0
{-# INLINE tcpNs  #-}
{-# INLINE tcpCwr #-}
{-# INLINE tcpEce #-}
{-# INLINE tcpUrg #-}
{-# INLINE tcpAck #-}
{-# INLINE tcpPsh #-}
{-# INLINE tcpRst #-}
{-# INLINE tcpSyn #-}
{-# INLINE tcpFin #-}


-- | The length of the fixed part of the TcpHeader, in 4-byte octets.
tcpFixedHeaderLength :: Int
tcpFixedHeaderLength  = 5

-- | Render a TcpHeader.  The checksum value is never rendered, as it is
-- expected to be calculated and poked in afterwords.
putTcpHeader :: Putter TcpHeader
putTcpHeader TcpHeader { .. } =
  do putTcpPort tcpSourcePort
     putTcpPort tcpDestPort
     putTcpSeqNum tcpSeqNum
     putTcpAckNum tcpAckNum
     let (optLen,padding) = tcpOptionsLength tcpOptions_
     putTcpControl (tcpFixedHeaderLength + optLen) tcpFlags_
     putWord16be tcpWindow
     putWord16be 0
     putWord16be tcpUrgentPointer
     mapM_ putTcpOption tcpOptions_
     replicateM_ padding (putTcpOptionTag OptTagEndOfOptions)

-- | Parse a TcpHeader.
getTcpHeader :: Get TcpHeader
getTcpHeader  = label "TcpHeader" $
  do tcpSourcePort <- getTcpPort
     tcpDestPort   <- getTcpPort
     tcpSeqNum     <- getTcpSeqNum
     tcpAckNum     <- getTcpAckNum

     -- data offset and flags
     tcpFlags_ <- getWord16be
     let dataOff = fromIntegral ((tcpFlags_ `shiftR` 12) .&. 0xf)

     tcpWindow        <- getWord16be
     tcpChecksum      <- getWord16be
     tcpUrgentPointer <- getWord16be

     -- options, not including the end-of-list option
     let optsLen = dataOff - tcpFixedHeaderLength
     opts <- label "options" (isolate (optsLen `shiftL` 2) getTcpOptions)
     let tcpOptions_ = filter (/= OptEndOfOptions) opts

     return $! TcpHeader { .. }

-- | Render out the @Word8@ that contains the Control field of the TcpHeader.
putTcpControl :: Int -> Word16 -> Put
putTcpControl hdrLenWords flags =
  putWord16be (fromIntegral (hdrLenWords) `shiftL` 12 .|. (flags .&. 0x1ff))


-- Tcp Options -----------------------------------------------------------------

class HasTcpOptions a where
  tcpOptions :: Lens' a [TcpOption]

instance HasTcpOptions TcpHeader where
  tcpOptions f TcpHeader { .. } =
    fmap (\opts -> TcpHeader { tcpOptions_ = opts, .. }) (f tcpOptions_)

instance HasTcpOptions [TcpOption] where
  tcpOptions = id

findTcpOption :: HasTcpOptions opts => TcpOptionTag -> opts -> Maybe TcpOption
findTcpOption tag opts = find p (view tcpOptions opts)
  where
  p opt = tag == tcpOptionTag opt

setTcpOption :: HasTcpOptions opts => TcpOption -> opts -> opts
setTcpOption opt = over tcpOptions loop
  where
  tag = tcpOptionTag opt

  loop [] = [opt]
  loop (o:opts)
    | tcpOptionTag o == tag = opt : opts
    | otherwise             = o : loop opts

setTcpOptions :: HasTcpOptions opts => [TcpOption] -> opts -> opts
setTcpOptions opts a = foldr setTcpOption a opts

data TcpOptionTag = OptTagEndOfOptions
                  | OptTagNoOption
                  | OptTagMaxSegmentSize
                  | OptTagWindowScaling
                  | OptTagSackPermitted
                  | OptTagSack
                  | OptTagTimestamp
                  | OptTagUnknown !Word8
                    deriving (Eq,Show)

getTcpOptionTag :: Get TcpOptionTag
getTcpOptionTag  =
  do ty <- getWord8
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
  putWord8 $! case tag of
    OptTagEndOfOptions   -> 0
    OptTagNoOption       -> 1
    OptTagMaxSegmentSize -> 2
    OptTagWindowScaling  -> 3
    OptTagSackPermitted  -> 4
    OptTagSack           -> 5
    OptTagTimestamp      -> 8
    OptTagUnknown ty     -> ty



data TcpOption = OptEndOfOptions
               | OptNoOption
               | OptMaxSegmentSize !Word16
               | OptWindowScaling !Word8
               | OptSackPermitted
               | OptSack [SackBlock]
               | OptTimestamp !Word32 !Word32
               | OptUnknown !Word8 !Word8 !S.ByteString
                 deriving (Show,Eq)

data SackBlock = SackBlock { sbLeft  :: !TcpSeqNum
                           , sbRight :: !TcpSeqNum
                           } deriving (Show,Eq)

tcpOptionTag :: TcpOption -> TcpOptionTag
tcpOptionTag OptEndOfOptions{}   = OptTagEndOfOptions
tcpOptionTag OptNoOption{}       = OptTagNoOption
tcpOptionTag OptMaxSegmentSize{} = OptTagMaxSegmentSize
tcpOptionTag OptSackPermitted{}  = OptTagSackPermitted
tcpOptionTag OptSack{}           = OptTagSack
tcpOptionTag OptWindowScaling{}  = OptTagWindowScaling
tcpOptionTag OptTimestamp{}      = OptTagTimestamp
tcpOptionTag (OptUnknown ty _ _) = OptTagUnknown ty

-- | Get the rendered length of a list of TcpOptions, in 4-byte words, and the
-- number of padding bytes required.  This rounds up to the nearest 4-byte word.
tcpOptionsLength :: [TcpOption] -> (Int,Int)
tcpOptionsLength opts
  | left == 0 = (len,0)
  | otherwise = (len + 1,4 - left)
  where
  (len,left) = F.foldl' (\acc o -> acc + tcpOptionLength o) 0 opts `quotRem` 4


tcpOptionLength :: TcpOption -> Int
tcpOptionLength OptEndOfOptions{}    = 1
tcpOptionLength OptNoOption{}        = 1
tcpOptionLength OptMaxSegmentSize{}  = 4
tcpOptionLength OptWindowScaling{}   = 3
tcpOptionLength OptSackPermitted{}   = 2
tcpOptionLength (OptSack bs)         = sackLength bs
tcpOptionLength OptTimestamp{}       = 10
tcpOptionLength (OptUnknown _ len _) = fromIntegral len


putTcpOption :: Putter TcpOption
putTcpOption opt =
  do putTcpOptionTag (tcpOptionTag opt)
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
  loop =
    do left <- remaining
       if left > 0
          then body
          else return []

  body =
    do opt <- getTcpOption
       case opt of

         OptEndOfOptions ->
           do skip =<< remaining
              return []

         _ ->
           do rest <- loop
              return (opt:rest)

getTcpOption :: Get TcpOption
getTcpOption  =
  do tag <- getTcpOptionTag
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
getMaxSegmentSize  = label "Max Segment Size" $ isolate 3 $
  do len <- getWord8
     unless (len == 4) (fail ("Unexpected length: " ++ show len))
     OptMaxSegmentSize `fmap` getWord16be

putMaxSegmentSize :: Putter Word16
putMaxSegmentSize w16 =
  do putWord8 4
     putWord16be w16

getSackPermitted :: Get TcpOption
getSackPermitted  = label "Sack Permitted" $ isolate 1 $
  do len <- getWord8
     unless (len == 2) (fail ("Unexpected length: " ++ show len))
     return OptSackPermitted

putSackPermitted :: Put
putSackPermitted  = putWord8 2

getSack :: Get TcpOption
getSack  = label "Sack" $
  do len <- getWord8
     let edgeLen = fromIntegral len - 2
     bs <- isolate edgeLen (replicateM (edgeLen `shiftR` 3) getSackBlock)
     return $! OptSack bs

putSack :: Putter [SackBlock]
putSack bs =
  do putWord8 (fromIntegral (sackLength bs))
     mapM_ putSackBlock bs

getSackBlock :: Get SackBlock
getSackBlock  =
  do l <- getTcpSeqNum
     r <- getTcpSeqNum
     return $! SackBlock { sbLeft  = l
                         , sbRight = r
                         }

putSackBlock :: Putter SackBlock
putSackBlock sb =
  do putTcpSeqNum (sbLeft sb)
     putTcpSeqNum (sbRight sb)

sackLength :: [SackBlock] -> Int
sackLength bs = length bs * 8 + 2

getWindowScaling :: Get TcpOption
getWindowScaling  = label "Window Scaling" $ isolate 2 $
  do len <- getWord8
     unless (len == 3) (fail ("Unexpected length: " ++ show len))
     OptWindowScaling `fmap` getWord8

putWindowScaling :: Putter Word8
putWindowScaling w =
  do putWord8 3
     putWord8 w

getTimestamp :: Get TcpOption
getTimestamp  = label "Timestamp" $ isolate 9 $
  do len <- getWord8
     unless (len == 10) (fail ("Unexpected length: " ++ show len))
     a <- getWord32be
     b <- getWord32be
     return $! OptTimestamp a b

putTimestamp :: Word32 -> Word32 -> Put
putTimestamp v r =
  do putWord8 10
     putWord32be v
     putWord32be r

getUnknown :: Word8 -> Get TcpOption
getUnknown ty =
  do len  <- getWord8
     body <- isolate (fromIntegral len - 2) (getBytes =<< remaining)
     return (OptUnknown ty len body)

putUnknown :: Word8 -> S.ByteString -> Put
putUnknown len body =
  do putWord8 len
     putByteString body


-- Tcp Packet ------------------------------------------------------------------

-- | The length of the data segment, including Syn and Fin.
tcpSegLen :: TcpHeader -> Int -> Int
tcpSegLen hdr len = len + flagVal tcpSyn + flagVal tcpFin
  where
  flagVal l | view l hdr = 1
            | otherwise  = 0

-- | The last sequence number used in a segment.
tcpSegLastSeqNum :: TcpHeader -> Int -> TcpSeqNum
tcpSegLastSeqNum hdr len = tcpSeqNum hdr + fromIntegral (tcpSegLen hdr len) - 1

-- | The ack number for this segment.
tcpSegNextAckNum :: TcpHeader -> Int -> TcpAckNum
tcpSegNextAckNum hdr len = tcpSeqNum hdr + fromIntegral (tcpSegLen hdr len)
