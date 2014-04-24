{-# LANGUAGE RecordWildCards #-}
module Hans.Message.Dns where

import Control.Monad
import Data.Bits
import Data.Char ( chr )
import Data.Int
import Data.Serialize
import Data.Word

import qualified Data.ByteString as S


-- Utilities -------------------------------------------------------------------

nullTerminatedString :: Get String
nullTerminatedString  =
  do char <- getWord8
     case char of
       0x0 -> return []
       _   -> do rest <- nullTerminatedString
                 return (chr (fromIntegral char) : rest)


-- DNS Packets -----------------------------------------------------------------

data DNSPacket = DNSPacket { dnsHeader            :: DNSHeader
                           , dnsQuestions         :: [RR]
                           , dnsAnswers           :: [RR]
                           , dnsAuthorityRecords  :: [RR]
                           , dnsAdditionalRecords :: [RR]
                           } deriving (Show)

getDNSPacket :: Get DNSPacket
getDNSPacket  =
  do dnsHeader <- getDNSHeader
     qdCount   <- getWord16be
     anCount   <- getWord16be
     nsCount   <- getWord16be
     arCount   <- getWord16be

     dnsQuestions         <- replicateM (fromIntegral qdCount) getRR
     dnsAnswers           <- replicateM (fromIntegral anCount) getRR
     dnsAuthorityRecords  <- replicateM (fromIntegral nsCount) getRR
     dnsAdditionalRecords <- replicateM (fromIntegral arCount) getRR

     return DNSPacket { .. }

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
         dnsOpCode = parseOpCode (fromIntegral (flags `shiftR` 11))
         dnsAA     = flags `testBit` 10
         dnsTC     = flags `testBit`  9
         dnsRD     = flags `testBit`  8
         dnsRA     = flags `testBit`  7
         dnsZ      = (flags `shiftR` 4) .&. 0x7
         dnsRC     = parseRespCode (fromIntegral (flags .&. 0xf))

     unless (dnsZ == 0) (fail ("Z not zero"))

     return DNSHeader { .. }

data OpCode = OpQuery
            | OpIQuery
            | OpStatus
            | OpReserved !Word8
              deriving (Show)

parseOpCode :: Word8 -> OpCode
parseOpCode 0 = OpQuery
parseOpCode 1 = OpIQuery
parseOpCode 2 = OpStatus
parseOpCode c = OpReserved (c .&. 0xf)

data RespCode = RespNoError
              | RespFormatError
              | RespServerFailure
              | RespNameError
              | RespNotImplemented
              | RespRefused
              | RespReserved !Word8
                deriving (Show)

parseRespCode :: Word8 -> RespCode
parseRespCode 0 = RespNoError
parseRespCode 1 = RespFormatError
parseRespCode 2 = RespServerFailure
parseRespCode 3 = RespNameError
parseRespCode 4 = RespNotImplemented
parseRespCode 5 = RespRefused
parseRespCode c = RespReserved (c .&. 0xf)


-- Resource Records ------------------------------------------------------------

data RR = RR { rrName  :: String
             , rrClass :: Class
             , rrTTL   :: !Int32
             , rrRData :: RData
             } deriving (Show)

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
getRR :: Get RR
getRR  =
  do rrName  <- nullTerminatedString
     ty      <- getWord16be
     rrClass <- getClass
     ttl     <- getWord32be
     let rrTTL = fromIntegral ttl
     rrRData <- getRData ty
     return RR { .. }

data Class = IN | CS | CH | HS
             deriving (Show,Eq)

getClass :: Get Class
getClass  =
  do tag <- getWord16be
     case tag of
       1 -> return IN
       2 -> return CS
       3 -> return CH
       4 -> return HS
       _ -> fail ("Invalid CLASS: " ++ show tag)


-- RDATA Formats ---------------------------------------------------------------

data RData = RDCName String
           | RDHInfo String String
           | RDUnknown Word16 S.ByteString
             deriving (Show)

getRData :: Word16 -> Get RData
getRData ty =
  do len <- getWord16be
     isolate (fromIntegral len) $ case ty of
       1  -> fail "A not implemented"
       2  -> fail "NS not implemented"
       3  -> fail "MD not implemented"
       4  -> fail "MF not implemented"
       5  -> fail "CNAME not implemented"
       6  -> fail "SOA not implemented"
       7  -> fail "MB not implemented"
       8  -> fail "MG not implemented"
       9  -> fail "MR not implemented"
       10 -> fail "NULL not implemented"
       11 -> fail "WKS not implemented"
       12 -> fail "PTR not implemented"
       13 -> fail "HINFO not implemented"
       14 -> fail "MINFO not implemented"
       15 -> fail "MX not implemented"
       _  -> RDUnknown ty `fmap` (getBytes =<< remaining)
