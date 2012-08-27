{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Icmp4 where

import Hans.Address.IP4 (IP4)
import Hans.Message.Types (Lifetime,parseLifetime,renderLifetime)
import Hans.Utils.Checksum (pokeChecksum, computeChecksum)

import Control.Monad (liftM2, unless, when, replicateM)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getWord8, getByteString, remaining, skip, Get, label,
                           lookAhead, getBytes, isEmpty)
import Data.Serialize.Put (Putter, putWord8,putByteString, Put, runPut)
import Data.Int (Int32)
import Data.Word (Word8,Word16,Word32)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S


-- General ICMP Packets --------------------------------------------------------

data Icmp4Packet
  -- RFC 792 - Internet Control Message Protocol
  = EchoReply Identifier SequenceNumber S.ByteString
  | DestinationUnreachable DestinationUnreachableCode S.ByteString
  | SourceQuench S.ByteString
  | Redirect RedirectCode IP4 S.ByteString
  | Echo Identifier SequenceNumber S.ByteString
  | RouterAdvertisement Lifetime [RouterAddress]
  | RouterSolicitation
  | TimeExceeded TimeExceededCode S.ByteString
  | ParameterProblem Word8 S.ByteString
  | Timestamp Identifier SequenceNumber Word32 Word32 Word32
  | TimestampReply Identifier SequenceNumber Word32 Word32 Word32
  | Information Identifier SequenceNumber
  | InformationReply Identifier SequenceNumber

  -- rfc 1393 - Traceroute Using an IP Option
  | TraceRoute TraceRouteCode Identifier Word16 Word16 Word32 Word32

  -- rfc 950 - Internet Standard Subnetting Procedure
  | AddressMask Identifier SequenceNumber
  | AddressMaskReply Identifier SequenceNumber Word32
  deriving (Eq,Show)

noCode :: String -> Get ()
noCode str = do
  code <- getWord8
  unless (code == 0)
    (fail (str ++ " expects code 0"))

parseIcmp4Packet :: Get Icmp4Packet
parseIcmp4Packet  = label "ICMP" $ do
  rest <- lookAhead (getBytes =<< remaining)
  unless (computeChecksum 0 rest == 0)
    (fail "Bad checksum")
  ty      <- get

  let firstGet :: Serialize a => String -> (a -> Get b) -> Get b
      firstGet labelString f = label labelString $ do
        code <- get
        skip 2 -- checksum
        f code

  case (ty :: Word8) of
    0  -> firstGet "Echo Reply" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             dat      <- getByteString =<< remaining
             return $! EchoReply ident seqNum dat

    3  -> firstGet "DestinationUnreachable" $ \ code -> do
             skip 4   -- unused
             dat      <- getByteString =<< remaining
             return $! DestinationUnreachable code dat

    4  -> firstGet "Source Quence" $ \ NoCode -> do
             skip 4   -- unused
             dat      <- getByteString =<< remaining
             return $! SourceQuench dat

    5  -> firstGet "Redirect" $ \ code -> do
             gateway  <- get
             dat      <- getByteString =<< remaining
             return $! Redirect code gateway dat

    8  -> firstGet "Echo" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             dat      <- getByteString =<< remaining
             return $! Echo ident seqNum dat

    9  -> firstGet "Router Advertisement" $ \ NoCode -> do
             n        <- getWord8
             sz       <- getWord8
             unless (sz == 2)
               (fail ("Expected size 2, got: " ++ show sz))
             lifetime <- parseLifetime
             addrs    <- replicateM (fromIntegral n) get
             return $! RouterAdvertisement lifetime addrs

    10 -> firstGet "Router Solicitation" $ \ NoCode -> do
             skip 4   -- reserved
             return RouterSolicitation

    11 -> firstGet "Time Exceeded" $ \ code -> do
             skip 4   -- unused
             dat      <- getByteString =<< remaining
             return $! TimeExceeded code dat

    12 -> firstGet "Parameter Problem" $ \ NoCode -> do
             ptr      <- getWord8
             skip 3   -- unused
             dat      <- getByteString =<< remaining
             return $! ParameterProblem ptr dat

    13 -> firstGet "Timestamp" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             origTime <- get
             recvTime <- get
             tranTime <- get
             return $! Timestamp ident seqNum origTime recvTime tranTime

    14 -> firstGet "Timestamp Reply" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             origTime <- get
             recvTime <- get
             tranTime <- get
             return $! TimestampReply ident seqNum origTime recvTime tranTime

    15 -> firstGet "Information" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             return $! Information ident seqNum

    16 -> firstGet "Information Reply" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             return $! InformationReply ident seqNum

    17 -> firstGet "Address Mask" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             skip 4   -- address mask
             return $! AddressMask ident seqNum

    18 -> firstGet "Address Mask Reply" $ \ NoCode -> do
             ident    <- get
             seqNum   <- get
             mask     <- get
             return $! AddressMaskReply ident seqNum mask

    30 -> firstGet "Trace Route" $ \ code -> do
             ident    <- get
             skip 2   -- unused
             outHop   <- get
             retHop   <- get
             speed    <- get
             mtu      <- get
             return $! TraceRoute code ident outHop retHop speed mtu

    _ -> fail ("Unknown type: " ++ show ty)


renderIcmp4Packet :: Putter Icmp4Packet
renderIcmp4Packet  =
      putByteString . unsafePerformIO . setChecksum . runPut . put'
                       -- Argument for safety: The bytestring being
                       -- destructively modified here is only accessible
                       -- through the composition and will never escape
  where
  setChecksum pkt = pokeChecksum (computeChecksum 0 pkt) pkt 2

  firstPut :: Serialize a => Word8 -> a -> Put
  firstPut ty code
    = do put ty
         put code
         put (0 :: Word16)

  put' (EchoReply ident seqNum dat)
    = do firstPut 0 NoCode
         put ident
         put seqNum
         putByteString dat

  put' (DestinationUnreachable code dat)
    = do firstPut 3 code
         put (0 :: Word32) -- unused
         putByteString dat

  put' (SourceQuench dat)
    = do firstPut 4 NoCode
         put (0 :: Word32) -- unused
         putByteString dat

  put' (Redirect code gateway dat)
    = do firstPut 5 code
         put gateway
         putByteString dat

  put' (Echo ident seqNum dat)
    = do firstPut 8 NoCode
         put ident
         put seqNum
         putByteString dat

  put' (RouterAdvertisement lifetime addrs)
    = do let len = length addrs
             addrSize :: Word8
             addrSize = 2

         when (len > 255)
           (fail "Too many routers in Router Advertisement")

         firstPut 9 NoCode
         put (fromIntegral len :: Word8)
         put addrSize
         renderLifetime lifetime
         mapM_ put addrs

  put' RouterSolicitation
    = do firstPut 10 NoCode
         put (0 :: Word32) -- RESERVED

  put' (TimeExceeded code dat)
    = do firstPut 11 code
         put (0 :: Word32) -- unused
         putByteString dat

  put' (ParameterProblem ptr dat)
    = do firstPut 12 NoCode
         put ptr
         put (0 :: Word8)  -- unused
         put (0 :: Word16) -- unused
         putByteString dat

  put' (Timestamp ident seqNum origTime recvTime tranTime)
    = do firstPut 13 NoCode
         put ident
         put seqNum
         put origTime
         put recvTime
         put tranTime

  put' (TimestampReply ident seqNum origTime recvTime tranTime)
    = do firstPut 14 NoCode
         put ident
         put seqNum
         put origTime
         put recvTime
         put tranTime

  put' (Information ident seqNum)
    = do firstPut 15 NoCode
         put ident
         put seqNum

  put' (InformationReply ident seqNum)
    = do firstPut 16 NoCode
         put ident
         put seqNum

  put' (AddressMask ident seqNum)
    = do firstPut 17 NoCode
         put ident
         put seqNum
         put (0 :: Word32) -- address mask

  put' (AddressMaskReply ident seqNum mask)
    = do firstPut 17 NoCode
         put ident
         put seqNum
         put mask

  put' (TraceRoute code ident outHop retHop speed mtu)
    = do firstPut 30 code
         put ident
         put (0 :: Word16) -- unused
         put outHop
         put retHop
         put speed
         put mtu

data NoCode = NoCode

instance Serialize NoCode where
  get = do b <- getWord8
           unless (b == 0)
             (fail ("Expected code 0, got code: " ++ show b))
           return NoCode
  put NoCode = putWord8 0

data DestinationUnreachableCode
  = NetUnreachable
  | HostUnreachable
  | ProtocolUnreachable
  | PortUnreachable
  | FragmentationUnreachable
  | SourceRouteFailed
  | DestinationNetworkUnknown
  | DestinationHostUnknown
  | SourceHostIsolatedError
  | AdministrativelyProhibited
  | HostAdministrativelyProhibited
  | NetworkUnreachableForTOS
  | HostUnreachableForTOS
  | CommunicationAdministrativelyProhibited
  | HostPrecedenceViolation
  | PrecedenceCutoffInEffect
  deriving (Eq,Show)

instance Serialize DestinationUnreachableCode where
  get = do b <- getWord8
           case b of
             0  -> return NetUnreachable
             1  -> return HostUnreachable
             2  -> return ProtocolUnreachable
             3  -> return PortUnreachable
             4  -> return FragmentationUnreachable
             5  -> return SourceRouteFailed
             6  -> return DestinationNetworkUnknown
             7  -> return DestinationHostUnknown
             8  -> return SourceHostIsolatedError
             9  -> return AdministrativelyProhibited
             10 -> return HostAdministrativelyProhibited
             11 -> return NetworkUnreachableForTOS
             12 -> return HostUnreachableForTOS
             13 -> return CommunicationAdministrativelyProhibited
             14 -> return HostPrecedenceViolation
             15 -> return PrecedenceCutoffInEffect
             _  -> fail "Invalid code for Destination Unreachable"

  put code = case code of
    NetUnreachable                          -> putWord8 0
    HostUnreachable                         -> putWord8 1
    ProtocolUnreachable                     -> putWord8 2
    PortUnreachable                         -> putWord8 3
    FragmentationUnreachable                -> putWord8 4
    SourceRouteFailed                       -> putWord8 5
    DestinationNetworkUnknown               -> putWord8 6
    DestinationHostUnknown                  -> putWord8 7
    SourceHostIsolatedError                 -> putWord8 8
    AdministrativelyProhibited              -> putWord8 9
    HostAdministrativelyProhibited          -> putWord8 10
    NetworkUnreachableForTOS                -> putWord8 11
    HostUnreachableForTOS                   -> putWord8 12
    CommunicationAdministrativelyProhibited -> putWord8 13
    HostPrecedenceViolation                 -> putWord8 14
    PrecedenceCutoffInEffect                -> putWord8 15

data TimeExceededCode
  = TimeToLiveExceededInTransit
  | FragmentReassemblyTimeExceeded
  deriving (Eq,Show)

instance Serialize TimeExceededCode where
  get = do b <- getWord8
           case b of
             0 -> return TimeToLiveExceededInTransit
             1 -> return FragmentReassemblyTimeExceeded
             _ -> fail "Invalid code for Time Exceeded"

  put TimeToLiveExceededInTransit       = putWord8 0
  put FragmentReassemblyTimeExceeded    = putWord8 1

data RedirectCode
  = RedirectForNetwork
  | RedirectForHost
  | RedirectForTypeOfServiceAndNetwork
  | RedirectForTypeOfServiceAndHost
  deriving (Eq,Show)

instance Serialize RedirectCode where
  get = do b <- getWord8
           case b of
             0 -> return RedirectForNetwork
             1 -> return RedirectForHost
             2 -> return RedirectForTypeOfServiceAndNetwork
             3 -> return RedirectForTypeOfServiceAndHost
             _ -> fail "Invalid code for Time Exceeded"

  put RedirectForNetwork                        = putWord8 0
  put RedirectForHost                           = putWord8 1
  put RedirectForTypeOfServiceAndNetwork        = putWord8 2
  put RedirectForTypeOfServiceAndHost           = putWord8 3

data TraceRouteCode
  = TraceRouteForwarded
  | TraceRouteDiscarded
  deriving (Eq,Show)

instance Serialize TraceRouteCode where
  get = do b <- getWord8
           case b of
             0 -> return TraceRouteForwarded
             1 -> return TraceRouteDiscarded
             _ -> fail "Invalid code for Trace Route"

  put TraceRouteForwarded       = putWord8 0
  put TraceRouteDiscarded       = putWord8 1

-- Router Discovery Data -------------------------------------------------------

newtype PreferenceLevel = PreferenceLevel Int32
  deriving (Show,Eq,Ord,Num,Serialize)

data RouterAddress = RouterAddress
  { raAddr            :: IP4
  , raPreferenceLevel :: PreferenceLevel
  } deriving (Eq,Show)

instance Serialize RouterAddress where
  get = liftM2 RouterAddress get get

  put ra = do
    put (raAddr            ra)
    put (raPreferenceLevel ra)

newtype Identifier = Identifier Word16
  deriving (Show, Eq, Ord, Num, Serialize)

newtype SequenceNumber = SequenceNumber Word16
  deriving (Show, Eq, Ord, Num, Serialize)

getUntilDone :: Serialize a => Get [a]
getUntilDone = do
  empty <- isEmpty
  if empty then return []
           else liftM2 (:) get getUntilDone
