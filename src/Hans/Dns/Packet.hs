
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Hans.Dns.Packet (
    DNSPacket(..)
  , DNSHeader(..)
  , OpCode(..)
  , RespCode(..)
  , Query(..)
  , QClass(..)
  , QType(..)
  , RR(..)
  , Type(..)
  , Class(..)
  , RData(..)
  , Name

  , getDNSPacket
  , putDNSPacket
  ) where

import Hans.IP4.Packet (IP4,getIP4,putIP4)

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as S
import qualified Data.Map.Strict as Map
import qualified Data.Serialize.Get as C
import qualified Data.Foldable as F
import           Data.Int
import           Data.Serialize ( Putter, runPut, putWord8, putWord16be, putWord32be
                                , putByteString )
import           Data.Word
import           MonadLib ( lift, StateT, runStateT, get, set )
import           Numeric ( showHex )



-- DNS Packets -----------------------------------------------------------------

data DNSPacket = DNSPacket { dnsHeader            :: DNSHeader
                           , dnsQuestions         :: [Query]
                           , dnsAnswers           :: [RR]
                           , dnsAuthorityRecords  :: [RR]
                           , dnsAdditionalRecords :: [RR]
                           } deriving (Show)

data DNSHeader = DNSHeader { dnsId     :: !Word16
                           , dnsQuery  :: Bool
                           , dnsOpCode :: OpCode
                           , dnsAA     :: Bool
                           , dnsTC     :: Bool
                           , dnsRD     :: Bool
                           , dnsRA     :: Bool
                           , dnsRC     :: RespCode
                           } deriving (Show)

data OpCode = OpQuery
            | OpIQuery
            | OpStatus
            | OpReserved !Word16
              deriving (Show)

data RespCode = RespNoError
              | RespFormatError
              | RespServerFailure
              | RespNameError
              | RespNotImplemented
              | RespRefused
              | RespReserved !Word16
                deriving (Eq,Show)

type Name = [S.ByteString]

data Query = Query { qName  :: Name
                   , qType  :: QType
                   , qClass :: QClass
                   } deriving (Show)

data RR = RR { rrName  :: Name
             , rrClass :: Class
             , rrTTL   :: !Int32
             , rrRData :: RData
             } deriving (Show)

data QType = QType Type
           | AFXR
           | MAILB
           | MAILA
           | QTAny
             deriving (Show)

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
          | PTR
          | HINFO
          | MINFO
          | MX
          | AAAA
            deriving (Show)

data QClass = QClass Class
            | QAnyClass
              deriving (Show)

data Class = IN | CS | CH | HS
             deriving (Show,Eq)

data RData = RDA IP4
           | RDNS Name
           | RDMD Name
           | RDMF Name
           | RDCNAME Name
           | RDSOA Name Name !Word32 !Int32 !Int32 !Int32 !Word32
           | RDMB Name
           | RDMG Name
           | RDMR Name
           | RDPTR Name
           | RDHINFO S.ByteString S.ByteString
           | RDMINFO Name Name
           | RDMX !Word16 Name
           | RDNULL S.ByteString
           | RDUnknown Type S.ByteString
             deriving (Show)


-- Cereal With Label Compression -----------------------------------------------

data RW = RW { rwOffset :: !Int
             , rwLabels :: Map.Map Int Name
             } deriving (Show)

type Get = StateT RW C.Get

{-# INLINE unGet #-}
unGet :: Get a -> C.Get a
unGet m =
  do (a,_) <- runStateT RW { rwOffset = 0, rwLabels = Map.empty } m
     return a

getOffset :: Get Int
getOffset  = rwOffset `fmap` get

addOffset :: Int -> Get ()
addOffset off =
  do rw <- get
     set $! rw { rwOffset = rwOffset rw + off }

lookupPtr :: Int -> Get Name
lookupPtr off =
  do rw <- get
     when (off >= rwOffset rw) (fail "Invalid offset in pointer")
     case Map.lookup off (rwLabels rw) of
       Just ls -> return ls
       Nothing -> fail $ "Unknown label for offset: " ++ showHex off "\n"
                      ++ show (rwLabels rw)

data Label = Label Int S.ByteString
           | Ptr Int Name
             deriving (Show)

labelsToName :: [Label] -> Name
labelsToName  = F.foldMap toName
  where
  toName (Label _ l) = [l]
  toName (Ptr _ n)   = n

addLabels :: [Label] -> Get ()
addLabels labels =
  do rw <- get
     set $! rw { rwLabels = Map.fromList newLabels `Map.union` rwLabels rw }
  where
  newLabels = go labels (labelsToName labels)

  go (Label off _ : rest) name@(_ : ns) = (off,name) : go rest ns
  go (Ptr off _   : _)    name          = [(off,name)]
  go _                    _             = []


{-# INLINE liftGet #-}
liftGet :: Int -> C.Get a -> Get a
liftGet n m = do addOffset n
                 lift m


{-# INLINE getWord8 #-}
getWord8 :: Get Word8
getWord8  = liftGet 1 C.getWord8

{-# INLINE getWord16be #-}
getWord16be :: Get Word16
getWord16be  = liftGet 2 C.getWord16be

{-# INLINE getWord32be #-}
getWord32be :: Get Word32
getWord32be  = liftGet 4 C.getWord32be

{-# INLINE getInt32be #-}
getInt32be :: Get Int32
getInt32be  = fromIntegral `fmap` liftGet 4 C.getWord32be

{-# INLINE getBytes #-}
getBytes :: Int -> Get S.ByteString
getBytes n = liftGet n (C.getBytes n)

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


{-# INLINE putInt32be #-}
putInt32be :: Putter Int32
putInt32be i = putWord32be (fromIntegral i)

-- Parsing ---------------------------------------------------------------------

getDNSPacket :: C.Get DNSPacket
getDNSPacket  = unGet $ label "DNSPacket" $
  do dnsHeader <- getDNSHeader
     qdCount   <- getWord16be
     anCount   <- getWord16be
     nsCount   <- getWord16be
     arCount   <- getWord16be

     let blockOf c l m = label l (replicateM (fromIntegral c) m)
     dnsQuestions         <- blockOf qdCount "Questions"          getQuery
     dnsAnswers           <- blockOf anCount "Answers"            getRR
     dnsAuthorityRecords  <- blockOf nsCount "Authority Records"  getRR
     dnsAdditionalRecords <- blockOf arCount "Additional Records" getRR

     return DNSPacket { .. }

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
     let dnsQuery  = not (flags `testBit` 15)
         dnsOpCode = parseOpCode (flags `shiftR` 11)
         dnsAA     = flags `testBit` 10
         dnsTC     = flags `testBit`  9
         dnsRD     = flags `testBit`  8
         dnsRA     = flags `testBit`  7
         dnsZ      = (flags `shiftR` 4) .&. 0x7
         dnsRC     = parseRespCode (flags .&. 0xf)

     unless (dnsZ == 0) (fail ("Z not zero"))

     return DNSHeader { .. }


parseOpCode :: Word16 -> OpCode
parseOpCode 0 = OpQuery
parseOpCode 1 = OpIQuery
parseOpCode 2 = OpStatus
parseOpCode c = OpReserved (c .&. 0xf)

parseRespCode :: Word16 -> RespCode
parseRespCode 0 = RespNoError
parseRespCode 1 = RespFormatError
parseRespCode 2 = RespServerFailure
parseRespCode 3 = RespNameError
parseRespCode 4 = RespNotImplemented
parseRespCode 5 = RespRefused
parseRespCode c = RespReserved (c .&. 0xf)

getQuery :: Get Query
getQuery  = label "Question" $
  do qName  <- getName
     qType  <- label "QTYPE"  getQType
     qClass <- label "QCLASS" getQClass
     return Query { .. }

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
getRR  = label "RR" $
  do rrName  <- getName
     ty      <- getType
     rrClass <- getClass
     rrTTL   <- getInt32be
     rrRData <- getRData ty
     return RR { .. }


getType :: Get Type
getType  =
  do qt <- getQType
     case qt of
       QType ty -> return ty
       _        -> fail ("Invalid TYPE: " ++ show qt)

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
       12  -> return (QType PTR)
       13  -> return (QType HINFO)
       14  -> return (QType MINFO)
       15  -> return (QType MX)
       28  -> return (QType AAAA)
       252 -> return AFXR
       253 -> return MAILB
       254 -> return MAILA
       255 -> return QTAny
       _   -> fail ("Invalid TYPE: " ++ show tag)


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

getName :: Get Name
getName  =
  do labels <- go
     addLabels labels
     return (labelsToName labels)
  where
  go = do off <- getOffset
          len <- getWord8
          if | len .&. 0xc0 == 0xc0 ->
               do l <- getWord8
                  let ptr = fromIntegral ((0x3f .&. len) `shiftL` 8)
                          + fromIntegral l
                  ns <- lookupPtr ptr
                  return [Ptr off ns]

             | len == 0 ->
                  return []

             | otherwise ->
               do l  <- getBytes (fromIntegral len)
                  ls <- go
                  return (Label off l:ls)

getClass :: Get Class
getClass  = label "CLASS" $
  do qc <- getQClass
     case qc of
       QClass c  -> return c
       QAnyClass -> fail "Invalid CLASS"

getRData :: Type -> Get RData
getRData ty = label (show ty) $
  do len <- getWord16be
     isolate (fromIntegral len) $ case ty of
       A     -> RDA  `fmap` liftGet 4 getIP4
       NS    -> RDNS `fmap` getName
       MD    -> RDMD `fmap` getName
       MF    -> RDMF `fmap` getName
       CNAME -> RDCNAME `fmap` getName
       SOA   -> do mname   <- getName
                   rname   <- getName
                   serial  <- getWord32be
                   refresh <- getInt32be
                   retry   <- getInt32be
                   expire  <- getInt32be
                   minTTL  <- getWord32be
                   return (RDSOA mname rname serial refresh retry expire minTTL)
       MB    -> RDMB `fmap` getName
       MG    -> RDMG `fmap` getName
       MR    -> RDMR `fmap` getName
       NULL  -> RDNULL `fmap` (getBytes =<< lift C.remaining)
       PTR   -> RDPTR `fmap` getName

       HINFO -> do cpuLen <- getWord8
                   cpu    <- getBytes (fromIntegral cpuLen)
                   osLen  <- getWord8
                   os     <- getBytes (fromIntegral osLen)
                   return (RDHINFO cpu os)

       MINFO -> do rmailBx <- getName
                   emailBx <- getName
                   return (RDMINFO rmailBx emailBx)

       MX    -> do pref <- getWord16be
                   ex   <- getName
                   return (RDMX pref ex)

       _     -> RDUnknown ty `fmap` (getBytes =<< lift C.remaining)


-- Rendering -------------------------------------------------------------------

putDNSPacket :: Putter DNSPacket
putDNSPacket DNSPacket{ .. } =
  do putDNSHeader dnsHeader

     putWord16be (fromIntegral (length dnsQuestions))
     putWord16be (fromIntegral (length dnsAnswers))
     putWord16be (fromIntegral (length dnsAuthorityRecords))
     putWord16be (fromIntegral (length dnsAdditionalRecords))

     F.traverse_ putQuery dnsQuestions
     F.traverse_ putRR dnsAnswers
     F.traverse_ putRR dnsAuthorityRecords
     F.traverse_ putRR dnsAdditionalRecords

putDNSHeader :: Putter DNSHeader
putDNSHeader DNSHeader { .. } =
  do putWord16be dnsId
     let flag i b w | b         = setBit w i
                    | otherwise = clearBit w i
         flags = flag 15 (not dnsQuery)
               $ flag 10 dnsAA
               $ flag  9 dnsTC
               $ flag  8 dnsRD
               $ flag  7 dnsRA
               $ flag  4 False -- dnsZ
               $ (renderOpCode dnsOpCode `shiftL` 11) .|. renderRespCode dnsRC
     putWord16be flags

renderOpCode :: OpCode -> Word16
renderOpCode OpQuery        = 0
renderOpCode OpIQuery       = 1
renderOpCode OpStatus       = 2
renderOpCode (OpReserved c) = c .&. 0xf

renderRespCode :: RespCode -> Word16
renderRespCode RespNoError        = 0
renderRespCode RespFormatError    = 1
renderRespCode RespServerFailure  = 2
renderRespCode RespNameError      = 3
renderRespCode RespNotImplemented = 4
renderRespCode RespRefused        = 5
renderRespCode (RespReserved c)   = c .&. 0xf

putName :: Putter Name
putName  = go
  where
  go (l:ls)
    | S.null l        = putWord8 0
    | S.length l > 63 = error "Label too big"
    | otherwise       = do putWord8 (fromIntegral len)
                           putByteString l
                           go ls
    where
    len = S.length l

  go [] = putWord8 0

putQuery :: Putter Query
putQuery Query { .. } =
  do putName   qName
     putQType  qType
     putQClass qClass

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
putType PTR   = putWord16be 12
putType HINFO = putWord16be 13
putType MINFO = putWord16be 14
putType MX    = putWord16be 15
putType AAAA  = putWord16be 28

putQType :: Putter QType
putQType (QType ty) = putType ty
putQType AFXR       = putWord16be 252
putQType MAILB      = putWord16be 253
putQType MAILA      = putWord16be 254
putQType QTAny      = putWord16be 255

putQClass :: Putter QClass
putQClass (QClass c) = putClass c
putQClass QAnyClass  = putWord16be 255

putRR :: Putter RR
putRR RR { .. } =
  do putName rrName
     let (ty,rdata) = putRData rrRData
     putType ty
     putClass rrClass
     putWord32be (fromIntegral rrTTL)
     putWord16be (fromIntegral (S.length rdata))
     putByteString rdata

putClass :: Putter Class
putClass IN = putWord16be 1
putClass CS = putWord16be 2
putClass CH = putWord16be 3
putClass HS = putWord16be 4

putRData :: RData -> (Type,S.ByteString)
putRData rd = case rd of
  RDA addr           -> rdata A     (putIP4 addr)
  RDNS name          -> rdata NS    (putName name)
  RDMD name          -> rdata MD    (putName name)
  RDMF name          -> rdata MF    (putName name)
  RDCNAME name       -> rdata CNAME (putName name)
  RDSOA m r s f t ex ttl ->
                        rdata SOA $ do putName m
                                       putName r
                                       putWord32be s
                                       putInt32be f
                                       putInt32be t
                                       putInt32be ex
                                       putWord32be ttl

  RDMB name          -> rdata MB    (putName name)
  RDMG name          -> rdata MG    (putName name)
  RDMR name          -> rdata MR    (putName name)
  RDNULL bytes       -> rdata NULL  $ do putWord8 (fromIntegral (S.length bytes))
                                         putByteString bytes
  RDPTR name         -> rdata PTR   (putName name)
  RDHINFO cpu os     -> rdata HINFO $ do putWord8 (fromIntegral (S.length cpu))
                                         putByteString cpu
                                         putWord8 (fromIntegral (S.length os))
                                         putByteString os
  RDMINFO rm em      -> rdata MINFO $ do putName rm
                                         putName em
  RDMX pref ex       -> rdata MX    $ do putWord16be pref
                                         putName ex
  RDUnknown ty bytes -> (ty,bytes)
  where
  rdata tag m = (tag,runPut m)
