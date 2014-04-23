{- | The 'Hans.Message.Dhcp4' module defines the various messages and
   transitions used in the DHCPv4 protocol. This module provides both
   a high-level view of the message types as well as a low-level
   intermediate form which is closely tied to the binary format.

   References:
   RFC 2131 - Dynamic Host Configuration Protocol
   http://www.faqs.org/rfcs/rfc2131.html
-}
module Hans.Message.Dhcp4
  (
  -- ** High-level client types
    RequestMessage(..)
  , Request(..)
  , Discover(..)

  -- ** High-level server types
  , ServerSettings(..)
  , ReplyMessage(..)
  , Ack(..)
  , Offer(..)

  -- ** Low-level message types
  , Dhcp4Message(..)
  , Xid(..)

  -- ** Server message transition logic
  , requestToAck
  , discoverToOffer

  -- ** Client message transition logic
  , mkDiscover
  , offerToRequest

  -- ** Convert high-level message types to low-level format
  , requestToMessage
  , ackToMessage
  , offerToMessage
  , discoverToMessage

  -- ** Convert low-level message type to high-level format
  , parseDhcpMessage

  -- ** Convert low-level message type to binary format
  , getDhcp4Message
  , putDhcp4Message

  ) where

import Hans.Address.IP4 (IP4(..))
import Hans.Address.Mac (Mac)
import Hans.Message.Dhcp4Codec
import Hans.Message.Dhcp4Options
import Hans.Utils (chunk)

import Control.Applicative ((<*), (<$>))
import Control.Monad (unless)
import Data.Bits (testBit,bit)
import Data.Maybe (mapMaybe)
import Data.Serialize.Get (Get, runGet, getByteString, isolate, remaining, label
                          , skip)
import Data.Serialize.Put (Put, runPut, putByteString)
import Data.Word (Word8,Word16,Word32)
import Numeric (showHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as L

-- DHCP Static Server Settings ---------------------------------------------

-- |'ServerSettings' define all of the information that would be needed to
-- act as a DHCP server for one client. The server is defined to be able to
-- issue a single "lease" whose parameters are defined below.
data ServerSettings = Settings
  { staticServerAddr :: IP4 -- ^ The IPv4 address of the DHCP server
  , staticTimeOffset :: Word32 -- ^ Lease: timezone offset in seconds from UTC
  , staticClientAddr :: IP4    -- ^ Lease: client IPv4 address on network
  , staticLeaseTime  :: Word32 -- ^ Lease: duration in seconds
  , staticSubnet     :: SubnetMask -- ^ Lease: subnet mask on network
  , staticBroadcast  :: IP4 -- ^ Lease: broadcast address on network
  , staticRouters    :: [IP4] -- ^ Lease: gateway routers on network
  , staticDomainName :: String -- ^ Lease: client's assigned domain name
  , staticDNS        :: [IP4] -- ^ Lease: network DNS servers
  }
  deriving (Show)

-- Structured DHCP Messages ------------------------------------------------

-- |'RequestMessage' is a sum of the client request messages.
data RequestMessage = RequestMessage Request
                    | DiscoverMessage Discover
  deriving (Show)

-- |'ReplyMessage' is a sum of the server response messages.
data ReplyMessage   = AckMessage Ack
                    | OfferMessage Offer
  deriving (Show)

-- |'Request' is used by the client to accept an offered lease.
data Request = Request
  { requestXid :: Xid -- ^ Transaction ID of offer
  , requestBroadcast :: Bool -- ^ Set 'True' to instruct server to send to broadcast hardware address
  , requestClientHardwareAddress :: Mac -- ^ Hardware address of the client
  , requestParameters :: [Dhcp4OptionTag] -- ^ Used to specify the information that client needs
  , requestAddress :: Maybe IP4 -- ^ Used to specify the address which was accepted
  }
  deriving (Show)

-- |'Discover' is used by the client to discover what servers are available.
-- This message is sent to the IPv4 broadcast.
data Discover = Discover
  { discoverXid :: Xid -- ^ Transaction ID of this and subsequent messages
  , discoverBroadcast :: Bool -- ^ Set 'True' to instruct the server to send to broadcast hardware address
  , discoverClientHardwareAddr :: Mac -- ^ Hardware address of the client
  , discoverParameters :: [Dhcp4OptionTag]-- ^ Used to specify the information that client needs in the offers
  }
  deriving (Show)

-- |'Ack' is sent by the DHCPv4 server to acknowledge a sucessful 'Request'
-- message. Upon receiving this message the client has completed the
-- exchange and has successfully obtained a lease.
data Ack = Ack
  { ackHops :: Word8 -- ^ The maximum number of relays this message can use.
  , ackXid  :: Xid   -- ^ Transaction ID for this exchange
  , ackYourAddr :: IP4 -- ^ Lease: assigned client address
  , ackServerAddr :: IP4 -- ^ DHCP server's IPv4 address
  , ackRelayAddr :: IP4 -- ^ DHCP relay server's address
  , ackClientHardwareAddr :: Mac -- ^ Client's hardware address
  , ackLeaseTime :: Word32 -- ^ Lease: duration of lease in seconds
  , ackOptions :: [Dhcp4Option] -- ^ Subset of information requested in previous 'Request'
  }
  deriving (Show)

-- |'Offer' is sent by the DHCPv4 server in response to a 'Discover'.
-- This offer is only valid for a short period of time as the client
-- might receive many offers. The client must next request a lease
-- from a specific server using the information in that server's offer.
data Offer = Offer
  { offerHops :: Word8 -- ^ The maximum number of relays this message can use.
  , offerXid  :: Xid -- ^ Transaction ID of this exchange
  , offerYourAddr :: IP4 -- ^ The IPv4 address that this server is willing to lease
  , offerServerAddr :: IP4 -- ^ The IPv4 address of the DHCPv4 server
  , offerRelayAddr :: IP4 -- ^ The IPv4 address of the DHCPv4 relay server
  , offerClientHardwareAddr :: Mac -- ^ The hardware address of the client
  , offerOptions :: [Dhcp4Option] -- ^ The options that this server would include in a lease
  }
  deriving (Show)

-- |'requestToAck' creates 'Ack' messages suitable for responding to 'Request'
-- messages given a static 'ServerSettings' configuration.
requestToAck :: ServerSettings -- ^ DHCPv4 server settings
             -> Request        -- ^ Client's request message
             -> Ack
requestToAck settings request = Ack
  { ackHops             = 1
  , ackXid              = requestXid request
  , ackYourAddr         = staticClientAddr settings
  , ackServerAddr       = staticServerAddr settings
  , ackRelayAddr        = staticServerAddr settings
  , ackClientHardwareAddr = requestClientHardwareAddress request
  , ackLeaseTime        = staticLeaseTime settings
  , ackOptions          = mapMaybe lookupOption (requestParameters request)
  }
  where
  lookupOption tag = case tag of
     OptTagSubnetMask
       -> Just (OptSubnetMask (staticSubnet settings))
     OptTagBroadcastAddress
       -> Just (OptBroadcastAddress (staticBroadcast settings))
     OptTagTimeOffset
       -> Just (OptTimeOffset (staticTimeOffset settings))
     OptTagRouters
       -> Just (OptRouters (staticRouters settings))
     OptTagDomainName
       -> Just (OptDomainName (NVTAsciiString (staticDomainName settings)))
     OptTagNameServers
       -> Just (OptNameServers (staticDNS settings))
     _ -> Nothing

-- |'discoverToOffer' creates a suitable 'Offer' in response to a client's
-- 'Discover' message using the configuration settings specified in the
-- given 'ServerSettings'.
discoverToOffer :: ServerSettings -- ^ DHCPv4 server settings
                -> Discover -- ^ Client's discover message
                -> Offer
discoverToOffer settings discover = Offer
  { offerHops             = 1
  , offerXid              = discoverXid discover
  , offerYourAddr         = staticClientAddr settings
  , offerServerAddr       = staticServerAddr settings
  , offerRelayAddr        = staticServerAddr settings
  , offerClientHardwareAddr = discoverClientHardwareAddr discover
  , offerOptions          = mapMaybe lookupOption (discoverParameters discover)
  }
  where
  lookupOption tag = case tag of
     OptTagSubnetMask
       -> Just (OptSubnetMask (staticSubnet settings))
     OptTagBroadcastAddress
       -> Just (OptBroadcastAddress (staticBroadcast settings))
     OptTagTimeOffset
       -> Just (OptTimeOffset (staticTimeOffset settings))
     OptTagRouters
       -> Just (OptRouters (staticRouters settings))
     OptTagDomainName
       -> Just (OptDomainName (NVTAsciiString (staticDomainName settings)))
     OptTagNameServers
       -> Just (OptNameServers (staticDNS settings))
     _ -> Nothing

-- Unstructured DHCP Message -----------------------------------------------

-- |'Dhcp4Message' is a low-level message container that is very close to
-- the binary representation of DHCPv4 message. It is suitable for containing
-- any DHCPv4 message. Values of this type should only be created using the
-- publicly exported functions.
data Dhcp4Message = Dhcp4Message
  { dhcp4Op                 :: Dhcp4Op -- ^ Message op code / message type. 1 = BOOTREQUEST, 2 = BOOTREPLY
  , dhcp4Hops               :: Word8 -- ^ Client sets to zero, optionally used by relay agents when booting via a relay agent.
  , dhcp4Xid                :: Xid -- ^ Transaction ID, a random number chosen by the client, used by the client and server to associate messages and responses between a client and a server.
  , dhcp4Secs               :: Word16 -- ^ Filled in by client, seconds elapsed since client began address acquisition or renewal process.
  , dhcp4Broadcast          :: Bool -- ^ Client requests messages be sent to hardware broadcast address
  , dhcp4ClientAddr         :: IP4 -- ^ Client IP address; only filled in if client is in BOUND, RENEW or REBINDING state and can respond to ARP requests.
  , dhcp4YourAddr           :: IP4 -- ^ 'your' (client) address
  , dhcp4ServerAddr         :: IP4 -- ^ IP address of next server to use in bootstrap; returned in DHCPOFFER, DHCPACK by server
  , dhcp4RelayAddr          :: IP4 -- ^ Relay agent IP address, used in booting via a relay agent
  , dhcp4ClientHardwareAddr :: Mac -- ^ Client hardware address
  , dhcp4ServerHostname     :: String -- ^ Optional server host name, null terminated string
  , dhcp4BootFilename       :: String -- ^ Boot file name, full terminated string; "generic" name of null in DHCPDISCOVER, fully qualified directory-path name in DHCPOFFER
  , dhcp4Options            :: [Dhcp4Option] -- ^ Optional parameters field.
  } deriving (Eq,Show)

-- |'getDhcp4Message' is the binary decoder for parsing 'Dhcp4Message' values.
getDhcp4Message :: BS.ByteString -> Either String Dhcp4Message
getDhcp4Message = runGet $ do
    op     <- getAtom
    hwtype <- getAtom
    len    <- getAtom
    unless (len == hardwareTypeAddressLength hwtype)
      (fail "Hardware address length does not match hardware type.")
    hops        <- label "hops" getAtom
    xid         <- label "xid" getAtom
    secs        <- label "secs" getAtom
    flags       <- label "flags" getAtom
    ciaddr      <- label "ciaddr" getAtom
    yiaddr      <- label "yiaddr" getAtom
    siaddr      <- label "siaddr" getAtom
    giaddr      <- label "giaddr" getAtom
    chaddr      <- label "chaddr" $ isolate 16 $ getAtom <* (skip =<< remaining)
    snameBytes  <- label "sname field" (getByteString 64)
    fileBytes   <- label "file field" (getByteString 128)
    (sname, file, opts) <- getDhcp4Options snameBytes fileBytes
    return $! Dhcp4Message
      { dhcp4Op                         = op
      , dhcp4Hops                       = hops
      , dhcp4Xid                        = xid
      , dhcp4Secs                       = secs
      , dhcp4Broadcast                  = broadcastFlag flags
      , dhcp4ClientAddr                 = ciaddr
      , dhcp4YourAddr                   = yiaddr
      , dhcp4ServerAddr                 = siaddr
      , dhcp4RelayAddr                  = giaddr
      , dhcp4ClientHardwareAddr         = chaddr
      , dhcp4ServerHostname             = sname
      , dhcp4BootFilename               = file
      , dhcp4Options                    = opts
      }

-- |'getDhcp4Message' is the binary encoder for rendering 'Dhcp4Message' values.
putDhcp4Message :: Dhcp4Message -> L.ByteString
putDhcp4Message dhcp = chunk $ runPut $ do
    putAtom (dhcp4Op dhcp)
    let hwType = Ethernet
    putAtom hwType
    putAtom (hardwareTypeAddressLength hwType)
    putAtom (dhcp4Hops dhcp)
    putAtom (dhcp4Xid  dhcp)
    putAtom (dhcp4Secs dhcp)
    putAtom Flags { broadcastFlag = dhcp4Broadcast dhcp }
    putAtom (dhcp4ClientAddr dhcp)
    putAtom (dhcp4YourAddr dhcp)
    putAtom (dhcp4ServerAddr dhcp)
    putAtom (dhcp4RelayAddr dhcp)
    putAtom (dhcp4ClientHardwareAddr dhcp)
    putByteString $ BS.replicate (16 {- chaddr field length -}
                        - fromIntegral (hardwareTypeAddressLength hwType)) 0
    putPaddedByteString 64 (BS8.pack (dhcp4ServerHostname dhcp))
    putPaddedByteString 128 (BS8.pack (dhcp4BootFilename dhcp))
    putDhcp4Options (dhcp4Options dhcp)

-- Transaction ID --------------------------------------------------------------

-- |'Xid' is a Transaction ID, a random number chosen by the client,
-- used by the client and server to associate messages and responses between a
-- client and a server.
newtype Xid = Xid Word32
 deriving (Eq, Show)

instance CodecAtom Xid where
  getAtom               = Xid <$> getAtom
  putAtom (Xid xid)     = putAtom xid
  atomSize _            = atomSize (0 :: Word32)

-- Opcodes ---------------------------------------------------------------------

data Dhcp4Op
  = BootRequest
  | BootReply
  deriving (Eq,Show)

instance CodecAtom Dhcp4Op where
  getAtom = do
    b <- getAtom
    case b :: Word8 of
      1 -> return BootRequest
      2 -> return BootReply
      _ -> fail ("Unknown DHCP op 0x" ++ showHex b "")

  putAtom BootRequest = putAtom (0x1 :: Word8)
  putAtom BootReply   = putAtom (0x2 :: Word8)

  atomSize _ = atomSize (0 :: Word8)

-- | HardwareType is an enumeration of the supported hardware types as assigned
--   in the ARP RFC http://www.iana.org/assignments/arp-parameters/
data HardwareType
  = Ethernet
  deriving (Eq, Show)

instance CodecAtom HardwareType where
  getAtom = getAtom >>= \ b -> case b :: Word8 of
      1 -> return Ethernet
      _ -> fail ("Unsupported hardware type 0x" ++ showHex b "")

  putAtom Ethernet = putAtom (1 :: Word8)

  atomSize _ = atomSize (1 :: Word8)

hardwareTypeAddressLength :: HardwareType -> Word8
hardwareTypeAddressLength Ethernet = 6

-- RFC 2131, Section 2, Page 11:
--                      1 1 1 1 1 1
--  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- |B|              MBZ            |
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- B:   BROADCAST flag
-- MBZ:   MUST BE ZERO (reserved for future use)
-- Figure 2: Format of the ’flags’ field

data Flags = Flags { broadcastFlag :: Bool }
  deriving (Show, Eq)

instance CodecAtom Flags where
  getAtom = do
    b <- getAtom :: Get Word16
    return Flags { broadcastFlag = testBit b 15 }

  putAtom flags = putAtom $ if broadcastFlag flags then bit 15 :: Word16
                                                   else 0

  atomSize _ = atomSize (0 :: Word16)

putPaddedByteString :: Int -> BS.ByteString -> Put
putPaddedByteString n bs = do
  putByteString $ BS.take n bs
  putByteString $ BS.replicate (n - BS.length bs) 0


selectKnownTags :: [OptionTagOrError] -> [Dhcp4OptionTag]
selectKnownTags = mapMaybe aux
 where
  aux (KnownTag t) = Just t
  aux _ = Nothing

--
-- Unstructured to Structured logic
--
-- |'parseDhcpMessage' attempts to find a valid high-level message
-- contained in a low-level message. The 'Dhcp4Message' is a large
-- type and can encode invalid combinations of options.
parseDhcpMessage :: Dhcp4Message -> Maybe (Either RequestMessage ReplyMessage)
parseDhcpMessage msg = do
  messageType <- lookupMessageType (dhcp4Options msg)
  case dhcp4Op msg of

    BootRequest -> Left <$> case messageType of

      Dhcp4Request -> RequestMessage <$> do
        params <- lookupParams (dhcp4Options msg)
        let params' = selectKnownTags params
        let addr = lookupRequestAddr (dhcp4Options msg)
        return Request
          { requestXid                          = dhcp4Xid msg
          , requestBroadcast                    = dhcp4Broadcast msg
          , requestClientHardwareAddress        = dhcp4ClientHardwareAddr msg
          , requestParameters                   = params'
          , requestAddress                      = addr
          }

      Dhcp4Discover -> DiscoverMessage <$> do
        params <- lookupParams (dhcp4Options msg)
        let params' = selectKnownTags params
        return Discover
          { discoverXid                         = dhcp4Xid msg
          , discoverBroadcast                   = dhcp4Broadcast msg
          , discoverClientHardwareAddr          = dhcp4ClientHardwareAddr msg
          , discoverParameters                  = params'
          }

      _ -> Nothing

    BootReply -> Right <$> case messageType of

      Dhcp4Ack -> AckMessage <$> do
        leaseTime <- lookupLeaseTime (dhcp4Options msg)
        return Ack
          { ackHops                             = dhcp4Hops msg
          , ackXid                              = dhcp4Xid msg
          , ackYourAddr                         = dhcp4YourAddr msg
          , ackServerAddr                       = dhcp4ServerAddr msg
          , ackRelayAddr                        = dhcp4RelayAddr msg
          , ackClientHardwareAddr               = dhcp4ClientHardwareAddr msg
          , ackLeaseTime                        = leaseTime
          , ackOptions                          = dhcp4Options msg
          }

      Dhcp4Offer -> OfferMessage <$> do
        return Offer
          { offerHops                           = dhcp4Hops msg
          , offerXid                            = dhcp4Xid msg
          , offerYourAddr                       = dhcp4YourAddr msg
          , offerServerAddr                     = dhcp4ServerAddr msg
          , offerRelayAddr                      = dhcp4RelayAddr msg
          , offerClientHardwareAddr             = dhcp4ClientHardwareAddr msg
          , offerOptions                        = dhcp4Options msg
          }

      _ -> Nothing

--
-- Structured to unstrucured logic
--
-- |'discoverToMessage' embeds 'Discover' messages in the low-level
-- 'Dhcp4Message' type, typically for the purpose of serialization.
discoverToMessage :: Discover -> Dhcp4Message
discoverToMessage discover = Dhcp4Message
  { dhcp4Op                     = BootRequest
  , dhcp4Hops                   = 0
  , dhcp4Xid                    = discoverXid discover
  , dhcp4Secs                   = 0
  , dhcp4Broadcast              = False
  , dhcp4ClientAddr             = IP4 0 0 0 0
  , dhcp4YourAddr               = IP4 0 0 0 0
  , dhcp4ServerAddr             = IP4 0 0 0 0
  , dhcp4RelayAddr              = IP4 0 0 0 0
  , dhcp4ClientHardwareAddr     = discoverClientHardwareAddr discover
  , dhcp4ServerHostname         = ""
  , dhcp4BootFilename           = ""
  , dhcp4Options                = [ OptMessageType Dhcp4Discover
                                  , OptParameterRequestList
                                      $ map KnownTag
                                      $ discoverParameters discover
                                  ]
  }

-- |'ackToMessage' embeds 'Ack' messages in the low-level
-- 'Dhcp4Message' type, typically for the purpose of serialization.
ackToMessage :: Ack -> Dhcp4Message
ackToMessage ack = Dhcp4Message
  { dhcp4Op                     = BootReply
  , dhcp4Hops                   = ackHops ack
  , dhcp4Xid                    = ackXid ack
  , dhcp4Secs                   = 0
  , dhcp4Broadcast              = False
  , dhcp4ClientAddr             = IP4 0 0 0 0
  , dhcp4YourAddr               = ackYourAddr ack
  , dhcp4ServerAddr             = ackServerAddr ack
  , dhcp4RelayAddr              = ackRelayAddr ack
  , dhcp4ClientHardwareAddr     = ackClientHardwareAddr ack
  , dhcp4ServerHostname         = ""
  , dhcp4BootFilename           = ""
  , dhcp4Options                = OptMessageType Dhcp4Ack
                                : OptServerIdentifier (ackServerAddr ack)
                                : OptIPAddressLeaseTime (ackLeaseTime ack)
                                : ackOptions ack
  }

-- |'offerToMessage' embeds 'Offer' messages in the low-level
-- 'Dhcp4Message' type, typically for the purpose of serialization.
offerToMessage :: Offer -> Dhcp4Message
offerToMessage offer = Dhcp4Message
  { dhcp4Op                     = BootReply
  , dhcp4Hops                   = offerHops offer
  , dhcp4Xid                    = offerXid offer
  , dhcp4Secs                   = 0
  , dhcp4Broadcast              = False
  , dhcp4ClientAddr             = IP4 0 0 0 0
  , dhcp4YourAddr               = offerYourAddr offer
  , dhcp4ServerAddr             = offerServerAddr offer
  , dhcp4RelayAddr              = offerRelayAddr offer
  , dhcp4ClientHardwareAddr     = offerClientHardwareAddr offer
  , dhcp4ServerHostname         = ""
  , dhcp4BootFilename           = ""
  , dhcp4Options                = OptMessageType Dhcp4Offer
                                : OptServerIdentifier (offerServerAddr offer)
                                : offerOptions offer
  }

-- |'requestToMessage' embeds 'Request' messages in the low-level
-- 'Dhcp4Message' type, typically for the purpose of serialization.
requestToMessage :: Request -> Dhcp4Message
requestToMessage request = Dhcp4Message
  { dhcp4Op                     = BootRequest
  , dhcp4Hops                   = 0
  , dhcp4Xid                    = requestXid request
  , dhcp4Secs                   = 0
  , dhcp4Broadcast              = requestBroadcast request
  , dhcp4ClientAddr             = IP4 0 0 0 0
  , dhcp4YourAddr               = IP4 0 0 0 0
  , dhcp4ServerAddr             = IP4 0 0 0 0
  , dhcp4RelayAddr              = IP4 0 0 0 0
  , dhcp4ClientHardwareAddr     = requestClientHardwareAddress request
  , dhcp4ServerHostname         = ""
  , dhcp4BootFilename           = ""
  , dhcp4Options                = [ OptMessageType Dhcp4Request
                                  , OptParameterRequestList
                                       $ map KnownTag
                                       $ requestParameters request
                                  ] ++ maybe [] (\x -> [OptRequestIPAddress x])
                                        (requestAddress request)
  }

-- |'mkDiscover' creates a new 'Discover' message with a set
-- of options suitable for configuring a basic network stack.
mkDiscover :: Xid -- ^ New randomly generated transaction ID
           -> Mac -- ^ The client's hardware address
           -> Discover
mkDiscover xid mac = Discover
   { discoverXid                = xid
   , discoverBroadcast          = False
   , discoverClientHardwareAddr = mac
   , discoverParameters         = [ OptTagSubnetMask
                                  , OptTagBroadcastAddress
                                  , OptTagTimeOffset
                                  , OptTagRouters
                                  , OptTagDomainName
                                  , OptTagNameServers
                                  , OptTagHostName
                                  ]
   }

-- |'offerToRequest' creates a 'Request' message suitable for accepting
-- an 'Offer' from the DHCPv4 server.
offerToRequest :: Offer -- ^ The offer as received from the server
               -> Request
offerToRequest offer = Request
  { requestXid                          = offerXid offer
  , requestBroadcast                    = False
  , requestClientHardwareAddress        = offerClientHardwareAddr offer
  , requestParameters                   = [ OptTagSubnetMask
                                          , OptTagBroadcastAddress
                                          , OptTagTimeOffset
                                          , OptTagRouters
                                          , OptTagDomainName
                                          , OptTagNameServers
                                          , OptTagHostName
                                          ]
  , requestAddress                      = Just (offerYourAddr offer)
  }
