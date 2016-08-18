{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.Icmp4 where

import Hans.Addr (IP4,getIP4,putIP4)
import Hans.Checksum (computeChecksum)
import Hans.Device.Types (ChecksumOffload(..))
import Hans.Serialize (runPutPacket)

import           Control.Monad (unless,when,replicateM,liftM2)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int32)
import           Data.Serialize (Serialize(..))
import           Data.Serialize.Get
                     (Get,getWord8,getWord16be,getWord32be,getInt32be,label
                     ,getByteString,skip,remaining,isEmpty)
import           Data.Serialize.Put
                     (Put,Putter,putWord8,putWord16be,putWord32be,putByteString)
import           Data.Word (Word8,Word16,Word32)


-- General ICMP Packets --------------------------------------------------------

type Lifetime = Word16

getLifetime :: Get Lifetime
getLifetime  = getWord16be

putLifetime :: Putter Lifetime
putLifetime  = putWord16be


data Icmp4Packet
  -- RFC 792 - Internet Control Message Protocol
  = EchoReply !Identifier !SequenceNumber !S.ByteString
  | DestinationUnreachable !DestinationUnreachableCode !S.ByteString
  | SourceQuench !S.ByteString
  | Redirect !RedirectCode !IP4 !S.ByteString
  | Echo !Identifier !SequenceNumber !S.ByteString
  | RouterAdvertisement !Lifetime [RouterAddress]
  | RouterSolicitation
  | TimeExceeded !TimeExceededCode !S.ByteString
  | ParameterProblem !Word8 !S.ByteString
  | Timestamp !Identifier !SequenceNumber !Word32 !Word32 !Word32
  | TimestampReply !Identifier !SequenceNumber !Word32 !Word32 !Word32
  | Information !Identifier !SequenceNumber
  | InformationReply !Identifier !SequenceNumber

  -- rfc 1393 - Traceroute Using an IP Option
  | TraceRoute !TraceRouteCode !Identifier !Word16 !Word16 !Word32 !Word32

  -- rfc 950 - Internet Standard Subnetting Procedure
  | AddressMask !Identifier !SequenceNumber
  | AddressMaskReply !Identifier !SequenceNumber !Word32
  deriving (Eq,Show)

noCode :: String -> Get ()
noCode str = do
  code <- getWord8
  unless (code == 0)
      (fail (str ++ " expects code 0"))

getIcmp4Packet :: Get Icmp4Packet
getIcmp4Packet  = label "ICMP" $
  do ty <- getWord8

     let firstGet :: Serialize a => String -> (a -> Get b) -> Get b
         firstGet labelString f = label labelString $
           do code <- get
              skip 2 -- checksum
              f code

     case ty of
       0  -> firstGet "Echo Reply" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
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
                gateway  <- getIP4
                dat      <- getByteString =<< remaining
                return $! Redirect code gateway dat

       8  -> firstGet "Echo" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                dat      <- getByteString =<< remaining
                return $! Echo ident seqNum dat

       9  -> firstGet "Router Advertisement" $ \ NoCode -> do
                n        <- getWord8
                sz       <- getWord8
                unless (sz == 2)
                    (fail ("Expected size 2, got: " ++ show sz))
                lifetime <- getLifetime
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
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                origTime <- getWord32be
                recvTime <- getWord32be
                tranTime <- getWord32be
                return $! Timestamp ident seqNum origTime recvTime tranTime

       14 -> firstGet "Timestamp Reply" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                origTime <- getWord32be
                recvTime <- getWord32be
                tranTime <- getWord32be
                return $! TimestampReply ident seqNum origTime recvTime tranTime

       15 -> firstGet "Information" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                return $! Information ident seqNum

       16 -> firstGet "Information Reply" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                return $! InformationReply ident seqNum

       17 -> firstGet "Address Mask" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                skip 4   -- address mask
                return $! AddressMask ident seqNum

       18 -> firstGet "Address Mask Reply" $ \ NoCode -> do
                ident    <- getIdentifier
                seqNum   <- getSequenceNumber
                mask     <- getWord32be
                return $! AddressMaskReply ident seqNum mask

       30 -> firstGet "Trace Route" $ \ code -> do
                ident    <- getIdentifier
                skip 2   -- unused
                outHop   <- getWord16be
                retHop   <- getWord16be
                speed    <- getWord32be
                mtu      <- getWord32be
                return $! TraceRoute code ident outHop retHop speed mtu

       _ -> fail ("Unknown type: " ++ show ty)


-- NOTE: we may want to parameterize this on the MTU of the device that will
-- send the message, as it could fragment.
renderIcmp4Packet :: ChecksumOffload -> Icmp4Packet -> L.ByteString
renderIcmp4Packet ChecksumOffload { .. } pkt
  | coIcmp4   = bytes
  | otherwise = L.take 2 bytes `L.append`
                    runPutPacket 2 100 (L.drop 4 bytes) (putWord16be cs)

  where

  -- best guess mtu
  mtu   = 1500 - 20

  bytes = runPutPacket mtu mtu L.empty (putIcmp4Packet pkt)
  cs    = computeChecksum bytes


putIcmp4Packet :: Putter Icmp4Packet
putIcmp4Packet  = put'
  where
  firstPut :: Serialize a => Word8 -> a -> Put
  firstPut ty code
    = do putWord8 ty
         put code
         putWord16be 0

  put' (EchoReply ident seqNum dat)
    = do firstPut 0 NoCode
         putIdentifier ident
         putSequenceNumber seqNum
         putByteString dat

  put' (DestinationUnreachable code dat)
    = do firstPut 3 code
         putWord32be 0 -- unused
         putByteString dat

  put' (SourceQuench dat)
    = do firstPut 4 NoCode
         putWord32be 0 -- unused
         putByteString dat

  put' (Redirect code gateway dat)
    = do firstPut 5 code
         putIP4 gateway
         putByteString dat

  put' (Echo ident seqNum dat)
    = do firstPut 8 NoCode
         putIdentifier ident
         putSequenceNumber seqNum
         putByteString dat

  put' (RouterAdvertisement lifetime addrs)
    = do let len      = length addrs
             addrSize = 2

         when (len > 255)
             (fail "Too many routers in Router Advertisement")

         firstPut 9 NoCode
         putWord8 (fromIntegral len)
         putWord8 addrSize
         putLifetime lifetime
         mapM_ put addrs

  put' RouterSolicitation
    = do firstPut 10 NoCode
         putWord32be 0 -- RESERVED

  put' (TimeExceeded code dat)
    = do firstPut 11 code
         putWord32be 0 -- unused
         putByteString dat

  put' (ParameterProblem ptr dat)
    = do firstPut 12 NoCode
         put ptr
         putWord8    0 -- unused
         putWord16be 0 -- unused
         putByteString dat

  put' (Timestamp ident seqNum origTime recvTime tranTime)
    = do firstPut 13 NoCode
         putIdentifier     ident
         putSequenceNumber seqNum
         putWord32be       origTime
         putWord32be       recvTime
         putWord32be       tranTime

  put' (TimestampReply ident seqNum origTime recvTime tranTime)
    = do firstPut 14 NoCode
         putIdentifier     ident
         putSequenceNumber seqNum
         putWord32be       origTime
         putWord32be       recvTime
         putWord32be       tranTime

  put' (Information ident seqNum)
    = do firstPut 15 NoCode
         putIdentifier ident
         putSequenceNumber seqNum

  put' (InformationReply ident seqNum)
    = do firstPut 16 NoCode
         putIdentifier ident
         putSequenceNumber seqNum

  put' (AddressMask ident seqNum)
    = do firstPut 17 NoCode
         putIdentifier ident
         putSequenceNumber seqNum
         putWord32be 0 -- address mask

  put' (AddressMaskReply ident seqNum mask)
    = do firstPut 18 NoCode
         putIdentifier ident
         putSequenceNumber seqNum
         putWord32be mask

  put' (TraceRoute code ident outHop retHop speed mtu)
    = do firstPut 30 code
         putIdentifier ident
         putWord16be 0 -- unused
         putWord16be outHop
         putWord16be retHop
         putWord32be speed
         putWord32be mtu

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

  put NetUnreachable                          = putWord8 0
  put HostUnreachable                         = putWord8 1
  put ProtocolUnreachable                     = putWord8 2
  put PortUnreachable                         = putWord8 3
  put FragmentationUnreachable                = putWord8 4
  put SourceRouteFailed                       = putWord8 5
  put DestinationNetworkUnknown               = putWord8 6
  put DestinationHostUnknown                  = putWord8 7
  put SourceHostIsolatedError                 = putWord8 8
  put AdministrativelyProhibited              = putWord8 9
  put HostAdministrativelyProhibited          = putWord8 10
  put NetworkUnreachableForTOS                = putWord8 11
  put HostUnreachableForTOS                   = putWord8 12
  put CommunicationAdministrativelyProhibited = putWord8 13
  put HostPrecedenceViolation                 = putWord8 14
  put PrecedenceCutoffInEffect                = putWord8 15

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

  put TimeToLiveExceededInTransit    = putWord8 0
  put FragmentReassemblyTimeExceeded = putWord8 1

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

  put RedirectForNetwork                 = putWord8 0
  put RedirectForHost                    = putWord8 1
  put RedirectForTypeOfServiceAndNetwork = putWord8 2
  put RedirectForTypeOfServiceAndHost    = putWord8 3

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

type PreferenceLevel = Int32

data RouterAddress = RouterAddress { raAddr            :: IP4
                                   , raPreferenceLevel :: PreferenceLevel
                                   } deriving (Eq,Show)

instance Serialize RouterAddress where
  get =
    do raAddr            <- getIP4
       raPreferenceLevel <- getInt32be
       return RouterAddress { .. }

  put RouterAddress { .. } =
    do putIP4 raAddr
       putWord32be (fromIntegral raPreferenceLevel)

type Identifier = Word16

getIdentifier :: Get Identifier
getIdentifier  = getWord16be
{-# INLINE getIdentifier #-}

putIdentifier :: Putter Identifier
putIdentifier  = putWord16be
{-# INLINE putIdentifier #-}

type SequenceNumber = Word16

getSequenceNumber :: Get SequenceNumber
getSequenceNumber  = getWord16be
{-# INLINE getSequenceNumber #-}

putSequenceNumber :: Putter SequenceNumber
putSequenceNumber  = putWord16be
{-# INLINE putSequenceNumber #-}

getUntilDone :: Serialize a => Get [a]
getUntilDone  =
  do empty <- isEmpty
     if empty then return []
              else liftM2 (:) get getUntilDone

