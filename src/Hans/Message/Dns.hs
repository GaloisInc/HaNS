{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Hans.Message.Dns where

import Hans.Address.IP4

import Control.Monad
import Data.Bits
import Data.Foldable ( Foldable, traverse_, foldMap )
import Data.Int
import Data.Serialize ( Put, Putter, putWord8, putWord16be, putWord32be
                      , putByteString )
import Data.Traversable ( Traversable )
import Data.Word
import MonadLib ( lift, StateT, runStateT, get, set )

import qualified Data.ByteString as S
import qualified Data.Map as Map
import qualified Data.Serialize.Get as C

-- Cereal With Stream Offsets --------------------------------------------------

type Get = StateT Int C.Get

{-# INLINE unGet #-}
unGet :: Get a -> C.Get a
unGet m =
  do (a,_) <- runStateT 0 m
     return a

bytesRead :: Get Int
bytesRead  = get

{-# INLINE increment #-}
increment :: Int -> C.Get a -> Get a
increment n m =
  do off <- get
     a   <- lift m
     set $! off + n
     return a

getWord8 :: Get Word8
getWord8  = increment 1 C.getWord8

getWord16be :: Get Word16
getWord16be  = increment 2 C.getWord16be

getWord32be :: Get Word32
getWord32be  = increment 4 C.getWord32be

getBytes :: Int -> Get S.ByteString
getBytes n = increment n (C.getBytes n)

isolate :: Int -> Get a -> Get a
isolate n body =
  do off      <- get
     (a,off') <- lift (C.isolate n (runStateT off body))
     set off'
     return a

label :: String -> Get a -> Get a
label str m =
  do off      <- get
     (a,off') <- lift (C.label str (runStateT off m))
     set off'
     return a


-- DNS Packets -----------------------------------------------------------------

data DNSPacket name = DNSPacket { dnsHeader            :: DNSHeader
                                , dnsQuestions         :: [Query name]
                                , dnsAnswers           :: [RR name]
                                , dnsAuthorityRecords  :: [RR name]
                                , dnsAdditionalRecords :: [RR name]
                                } deriving (Show,Functor,Foldable,Traversable)

getDNSPacket :: C.Get (DNSPacket Name)
getDNSPacket  = unGet $ label "DNSPacket" $
  do dnsHeader <- getDNSHeader
     qdCount   <- getWord16be
     anCount   <- getWord16be
     nsCount   <- getWord16be
     arCount   <- getWord16be

     dnsQuestions         <- replicateM (fromIntegral qdCount) getQuery
     dnsAnswers           <- replicateM (fromIntegral anCount) getRR
     dnsAuthorityRecords  <- replicateM (fromIntegral nsCount) getRR
     dnsAdditionalRecords <- replicateM (fromIntegral arCount) getRR

     return DNSPacket { .. }

putDNSPacket :: Putter (DNSPacket Name)
putDNSPacket DNSPacket{ .. } =
  do putDNSHeader dnsHeader

     putWord16be (fromIntegral (length dnsQuestions))
     putWord16be (fromIntegral (length dnsAnswers))
     putWord16be (fromIntegral (length dnsAuthorityRecords))
     putWord16be (fromIntegral (length dnsAdditionalRecords))

     traverse_ putQuery dnsQuestions
     traverse_ putRR dnsAnswers
     traverse_ putRR dnsAuthorityRecords
     traverse_ putRR dnsAdditionalRecords

resolvePointers :: DNSPacket Name -> DNSPacket [String]
resolvePointers pkt = undefined


data DNSHeader = DNSHeader { dnsId     :: !Word16
                           , dnsQuery  :: Bool
                           , dnsOpCode :: OpCode
                           , dnsAA     :: Bool
                           , dnsTC     :: Bool
                           , dnsRD     :: Bool
                           , dnsRA     :: Bool
                           , dnsRC     :: RespCode
                           } deriving (Show)

--                                  1  1  1  1  1  1
--    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                      ID                       |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                    QDCOUNT                    |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                    ANCOUNT                    |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                    NSCOUNT                    |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                    ARCOUNT                    |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
getDNSHeader :: Get DNSHeader
getDNSHeader  = label "DNS Header" $
  do dnsId <- getWord16be
     flags <- getWord16be
     let dnsQuery  = flags `testBit` 15
         dnsOpCode = parseOpCode (flags `shiftR` 11)
         dnsAA     = flags `testBit` 10
         dnsTC     = flags `testBit`  9
         dnsRD     = flags `testBit`  8
         dnsRA     = flags `testBit`  7
         dnsZ      = (flags `shiftR` 4) .&. 0x7
         dnsRC     = parseRespCode (flags .&. 0xf)

     unless (dnsZ == 0) (fail ("Z not zero"))

     return DNSHeader { .. }

putDNSHeader :: Putter DNSHeader
putDNSHeader DNSHeader { .. } =
  do putWord16be dnsId
     let flag i b w | b         = setBit w i
                    | otherwise = clearBit w i
         flags = flag 15 dnsQuery
               $ flag 10 dnsAA
               $ flag  9 dnsTC
               $ flag  8 dnsRD
               $ flag  7 dnsRA
               $ flag  4 False -- dnsZ
               $ (renderOpCode dnsOpCode `shiftL` 11) .|. renderRespCode dnsRC
     putWord16be flags


data OpCode = OpQuery
            | OpIQuery
            | OpStatus
            | OpReserved !Word16
              deriving (Show)

parseOpCode :: Word16 -> OpCode
parseOpCode 0 = OpQuery
parseOpCode 1 = OpIQuery
parseOpCode 2 = OpStatus
parseOpCode c = OpReserved (c .&. 0xf)

renderOpCode :: OpCode -> Word16
renderOpCode OpQuery        = 0
renderOpCode OpIQuery       = 1
renderOpCode OpStatus       = 2
renderOpCode (OpReserved c) = c .&. 0xf

data RespCode = RespNoError
              | RespFormatError
              | RespServerFailure
              | RespNameError
              | RespNotImplemented
              | RespRefused
              | RespReserved !Word16
                deriving (Show)

parseRespCode :: Word16 -> RespCode
parseRespCode 0 = RespNoError
parseRespCode 1 = RespFormatError
parseRespCode 2 = RespServerFailure
parseRespCode 3 = RespNameError
parseRespCode 4 = RespNotImplemented
parseRespCode 5 = RespRefused
parseRespCode c = RespReserved (c .&. 0xf)

renderRespCode :: RespCode -> Word16
renderRespCode RespNoError        = 0
renderRespCode RespFormatError    = 1
renderRespCode RespServerFailure  = 2
renderRespCode RespNameError      = 3
renderRespCode RespNotImplemented = 4
renderRespCode RespRefused        = 5
renderRespCode (RespReserved c)   = c .&. 0xf


-- Utilities -------------------------------------------------------------------

data Label = Label Int S.ByteString
           | Ptr Int
             deriving (Show)

type Name = [Label]

getName :: Get Name
getName  =
  do off <- bytesRead
     len <- getWord8
     if | len .&. 0xc0 /= 0 ->
          do l <- getWord8
             return [Ptr $ fromIntegral ((0x3f .&. len) `shiftL` 8)
                         + fromIntegral l ]
        | len == 0 ->
             return []
        | otherwise ->
          do l  <- getBytes (fromIntegral len)
             ls <- getName
             return (Label off l:ls)

putName :: Putter Name
putName (Label _ bytes : ls) =
  do putWord8 (fromIntegral (S.length bytes))
     putByteString bytes
     putName ls
putName (Ptr off:_) =
  do putWord8 (0xc0 .|. (fromIntegral off `shiftR` 8))
     putWord8 (fromIntegral off)
putName [] =
     putWord8 0


-- Queries ---------------------------------------------------------------------

data Query name = Query { qName  :: name
                        , qType  :: QType
                        , qClass :: QClass
                        } deriving (Show,Functor,Foldable,Traversable)

getQuery :: Get (Query Name)
getQuery  = label "Question" $
  do qName  <- getName
     qType  <- label "QTYPE"  getQType
     qClass <- label "QCLASS" getQClass
     return Query { .. }

putQuery :: Putter (Query Name)
putQuery Query { .. } =
  do putName   qName
     putQType  qType
     putQClass qClass


data Type = A
          | NS
          | MD
          | MF
          | CNAME
          | SOA
          | MB
          | MG
          | MR
          | NULL
          | WKS
          | PTR
          | HINFO
          | MINFO
          | MX
            deriving (Show)

getType :: Get Type
getType  =
  do qt <- getQType
     case qt of
       QType ty -> return ty
       _        -> fail ("Invalid TYPE: " ++ show qt)

putType :: Putter Type
putType A     = putWord16be 1
putType NS    = putWord16be 2
putType MD    = putWord16be 3
putType MF    = putWord16be 4
putType CNAME = putWord16be 5
putType SOA   = putWord16be 6
putType MB    = putWord16be 7
putType MG    = putWord16be 8
putType MR    = putWord16be 9
putType NULL  = putWord16be 10
putType WKS   = putWord16be 11
putType PTR   = putWord16be 12
putType HINFO = putWord16be 13
putType MINFO = putWord16be 14
putType MX    = putWord16be 15

data QType = QType Type
           | AFXR
           | MAILB
           | MAILA
           | QTAny
             deriving (Show)

getQType :: Get QType
getQType  =
  do tag <- getWord16be
     case tag of
       1   -> return (QType A)
       2   -> return (QType NS)
       3   -> return (QType MD)
       4   -> return (QType MF)
       5   -> return (QType CNAME)
       6   -> return (QType SOA)
       7   -> return (QType MB)
       8   -> return (QType MG)
       9   -> return (QType MR)
       10  -> return (QType NULL)
       11  -> return (QType WKS)
       12  -> return (QType PTR)
       13  -> return (QType HINFO)
       14  -> return (QType MINFO)
       15  -> return (QType MX)
       252 -> return AFXR
       253 -> return MAILB
       254 -> return MAILA
       255 -> return QTAny
       _   -> fail ("Invalid TYPE: " ++ show tag)

putQType :: Putter QType
putQType (QType ty) = putType ty
putQType AFXR       = putWord16be 252
putQType MAILB      = putWord16be 253
putQType MAILA      = putWord16be 254
putQType QTAny      = putWord16be 255


data QClass = QClass Class
            | QAnyClass
              deriving (Show)

getQClass :: Get QClass
getQClass  =
  do tag <- getWord16be
     case tag of
       1   -> return (QClass IN)
       2   -> return (QClass CS)
       3   -> return (QClass CH)
       4   -> return (QClass HS)
       255 -> return QAnyClass
       _   -> fail ("Invalid CLASS: " ++ show tag)

putQClass :: Putter QClass
putQClass (QClass c) = putClass c
putQClass QAnyClass  = putWord16be 255


-- Resource Records ------------------------------------------------------------

data RR name = RR { rrName  :: name
                  , rrClass :: Class
                  , rrTTL   :: !Int32
                  , rrRData :: RData name
                  } deriving (Show,Functor,Foldable,Traversable)

--                                  1  1  1  1  1  1
--    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                                               |
--  /                                               /
--  /                      NAME                     /
--  |                                               |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                      TYPE                     |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                     CLASS                     |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                      TTL                      |
--  |                                               |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  |                   RDLENGTH                    |
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
--  /                     RDATA                     /
--  /                                               /
--  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
getRR :: Get (RR Name)
getRR  = label "RR" $
  do rrName  <- getName
     ty      <- getType
     rrClass <- getClass
     ttl     <- getWord32be
     let rrTTL = fromIntegral ttl
     rrRData <- getRData ty
     return RR { .. }

putRR :: Putter (RR Name)
putRR RR { .. } =
  do putName rrName
     let (ty,rd) = putRData rrRData
     putType ty
     putClass rrClass
     putWord32be (fromIntegral rrTTL)
     rd

data Class = IN | CS | CH | HS
             deriving (Show,Eq)

getClass :: Get Class
getClass  = label "CLASS" $
  do qc <- getQClass
     case qc of
       QClass c  -> return c
       QAnyClass -> fail "Invalid CLASS"

putClass :: Putter Class
putClass IN = putWord16be 1
putClass CS = putWord16be 2
putClass CH = putWord16be 3
putClass HS = putWord16be 4


-- RDATA Formats ---------------------------------------------------------------

data RData name = RDA IP4
                | RDCNAME name
                | RDHInfo String String
                  deriving (Show,Functor,Foldable,Traversable)

getRData :: Type -> Get (RData Name)
getRData ty =
  do len <- getWord16be
     isolate (fromIntegral len) $ case ty of
       A     -> RDA `fmap` lift parseIP4
       NS    -> fail "NS not implemented"
       MD    -> fail "MD not implemented"
       MF    -> fail "MF not implemented"
       CNAME -> RDCNAME `fmap` getName
       SOA   -> fail "SOA not implemented"
       MB    -> fail "MB not implemented"
       MG    -> fail "MG not implemented"
       MR    -> fail "MR not implemented"
       NULL  -> fail "NULL not implemented"
       WKS   -> fail "WKS not implemented"
       PTR   -> fail "PTR not implemented"
       HINFO -> fail "HINFO not implemented"
       MINFO -> fail "MINFO not implemented"
       MX    -> fail "MX not implemented"

putRData :: RData Name -> (Type,Put)
putRData (RDA addr) = (A,) $
  do putWord16be 4
     renderIP4 addr
putRData rd = error ("unimplemented " ++ show rd)


-- Test Data -------------------------------------------------------------------

testBytes :: S.ByteString
testBytes  = "}\SOH\129\128\NUL\SOH\NUL\ETX\NUL\NUL\NUL\NUL\ENQyahoo\ETXcom\NUL\NUL\SOH\NUL\SOH\192\f\NUL\SOH\NUL\SOH\NUL\NUL\SOHL\NUL\EOTb\139\183\CAN\192\f\NUL\SOH\NUL\SOH\NUL\NUL\SOHL\NUL\EOT\206\190$-\192\f\NUL\SOH\NUL\SOH\NUL\NUL\SOHL\NUL\EOTb\138\253m"
