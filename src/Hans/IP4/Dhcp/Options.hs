module Hans.IP4.Dhcp.Options where

import Hans.Addr (IP4,IP4Mask)
import Hans.IP4.Dhcp.Codec

import qualified Control.Applicative as A
import           Control.Monad (unless)
import           Data.Maybe (fromMaybe)
import           Data.Foldable (traverse_)
import qualified Data.Traversable as T
import           Data.Word (Word8, Word16, Word32)
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Numeric (showHex)


-----------------------------------------------------------------------
-- Magic constants ----------------------------------------------------
-----------------------------------------------------------------------

data MagicCookie = MagicCookie

dhcp4MagicCookie :: Word32
dhcp4MagicCookie = 0x63825363

instance CodecAtom MagicCookie where
  getAtom = do cookie <- getAtom
               unless (cookie == dhcp4MagicCookie)
                  (fail "Incorrect magic cookie.")
               return MagicCookie
  putAtom MagicCookie = putAtom dhcp4MagicCookie
  atomSize MagicCookie = atomSize dhcp4MagicCookie


-----------------------------------------------------------------------
-- DHCP option type and operations ------------------------------------
-----------------------------------------------------------------------

data Dhcp4Option
  = OptSubnetMask SubnetMask
  | OptTimeOffset Word32
  | OptRouters [IP4]
  | OptTimeServers [IP4]
  | OptIEN116NameServers [IP4]
  | OptNameServers [IP4]
  | OptLogServers [IP4]
  | OptCookieServers [IP4]
  | OptLPRServers [IP4]
  | OptImpressServers [IP4]
  | OptResourceLocationServers [IP4]
  | OptHostName NVTAsciiString
  | OptBootFileSize Word16
  | OptMeritDumpFile NVTAsciiString
  | OptDomainName NVTAsciiString
  | OptSwapServer IP4
  | OptRootPath NVTAsciiString
  | OptExtensionsPath NVTAsciiString
  | OptEnableIPForwarding Bool
  | OptEnableNonLocalSourceRouting Bool
  | OptPolicyFilters [IP4Mask]
  | OptMaximumDatagramReassemblySize Word16
  | OptDefaultTTL Word8
  | OptPathMTUAgingTimeout Word32
  | OptPathMTUPlateauTable [Word16]
  | OptInterfaceMTU Word16
  | OptAllSubnetsAreLocal Bool
  | OptBroadcastAddress IP4
  | OptPerformMaskDiscovery Bool
  | OptShouldSupplyMasks Bool
  | OptShouldPerformRouterDiscovery Bool
  | OptRouterSolicitationAddress IP4
  | OptStaticRoutes [(IP4,IP4)]
  | OptShouldNegotiateArpTrailers Bool
  | OptArpCacheTimeout Word32
  | OptUseRFC1042EthernetEncapsulation Bool
  | OptTcpDefaultTTL Word8
  | OptTcpKeepaliveInterval Word32
  | OptTcpKeepaliveUseGarbage Bool
  | OptNisDomainName NVTAsciiString
  | OptNisServers [IP4]
  | OptNtpServers [IP4]
  | OptVendorSpecific ByteString
  | OptNetBiosNameServers [IP4]
  | OptNetBiosDistributionServers [IP4]
  | OptNetBiosNodeType NetBiosNodeType
  | OptNetBiosScope NVTAsciiString
  | OptXWindowsFontServer [IP4]
  | OptXWindowsDisplayManagers [IP4]
  | OptNisPlusDomain NVTAsciiString
  | OptNisPlusServers [IP4]
  | OptSmtpServers [IP4]
  | OptPopServers [IP4]
  | OptNntpServers [IP4]
  | OptWwwServers [IP4]
  | OptFingerServers [IP4]
  | OptIrcServers [IP4]
  | OptStreetTalkServers [IP4]
  | OptStreetTalkDirectoryAssistanceServers [IP4]
  | OptFQDN NVTAsciiString -- RFC 4702
  | OptRequestIPAddress IP4
  | OptIPAddressLeaseTime Word32
  | OptOverload OverloadOption
  | OptTftpServer NVTAsciiString
  | OptBootfileName NVTAsciiString
  | OptMessageType Dhcp4MessageType
  | OptServerIdentifier IP4
  | OptParameterRequestList [OptionTagOrError]
  | OptErrorMessage NVTAsciiString
  | OptMaxDHCPMessageSize Word16
  | OptRenewalTime Word32
  | OptRebindingTime Word32
  | OptVendorClass NVTAsciiString
  | OptClientIdentifier ByteString
  | OptNetWareDomainName NVTAsciiString -- RFC 2242
  | OptNetWareInfo ByteString           -- RFC 2242
  | OptAutoconfiguration Bool -- RFC 2563
  deriving (Show,Eq)

getDhcp4Option :: Get (Either ControlTag Dhcp4Option)
getDhcp4Option = do
  mb_tag <- getOptionTag
  case mb_tag of
    UnknownTag t -> do xs <- getBytes =<< remaining
                       fail ("getDhcp4Option failed tag (" ++ show t ++ ") " ++ show xs)
    KnownTag tag -> do
      let r con = (Right . con) `fmap` getOption
      case tag of
        OptTagPad                           -> A.pure (Left ControlPad)
        OptTagEnd                           -> A.pure (Left ControlEnd)
        OptTagSubnetMask                    -> r OptSubnetMask
        OptTagTimeOffset                    -> r OptTimeOffset
        OptTagRouters                       -> r OptRouters
        OptTagTimeServers                   -> r OptTimeServers
        OptTagIEN116NameServers             -> r OptIEN116NameServers
        OptTagNameServers                   -> r OptNameServers
        OptTagLogServers                    -> r OptLogServers
        OptTagCookieServers                 -> r OptCookieServers
        OptTagLPRServers                    -> r OptLPRServers
        OptTagImpressServers                -> r OptImpressServers
        OptTagResourceLocationServers       -> r OptResourceLocationServers
        OptTagHostName                      -> r OptHostName
        OptTagBootFileSize                  -> r OptBootFileSize
        OptTagMeritDumpFile                 -> r OptMeritDumpFile
        OptTagDomainName                    -> r OptDomainName
        OptTagSwapServer                    -> r OptSwapServer
        OptTagRootPath                      -> r OptRootPath
        OptTagExtensionsPath                -> r OptExtensionsPath
        OptTagEnableIPForwarding            -> r OptEnableIPForwarding
        OptTagEnableNonLocalSourceRouting   -> r OptEnableNonLocalSourceRouting
        OptTagPolicyFilters                 -> r OptPolicyFilters
        OptTagMaximumDatagramReassemblySize -> r OptMaximumDatagramReassemblySize
        OptTagDefaultTTL                    -> r OptDefaultTTL
        OptTagPathMTUAgingTimeout           -> r OptPathMTUAgingTimeout
        OptTagPathMTUPlateauTable           -> r OptPathMTUPlateauTable
        OptTagInterfaceMTU                  -> r OptInterfaceMTU
        OptTagAllSubnetsAreLocal            -> r OptAllSubnetsAreLocal
        OptTagBroadcastAddress              -> r OptBroadcastAddress
        OptTagPerformMaskDiscovery          -> r OptPerformMaskDiscovery
        OptTagShouldSupplyMasks             -> r OptShouldSupplyMasks
        OptTagShouldPerformRouterDiscovery  -> r OptShouldPerformRouterDiscovery
        OptTagRouterSolicitationAddress     -> r OptRouterSolicitationAddress
        OptTagStaticRoutes                  -> r OptStaticRoutes
        OptTagShouldNegotiateArpTrailers    -> r OptShouldNegotiateArpTrailers
        OptTagArpCacheTimeout               -> r OptArpCacheTimeout
        OptTagUseRFC1042EthernetEncapsulation -> r OptUseRFC1042EthernetEncapsulation
        OptTagTcpDefaultTTL                 -> r OptTcpDefaultTTL
        OptTagTcpKeepaliveInterval          -> r OptTcpKeepaliveInterval
        OptTagTcpKeepaliveUseGarbage        -> r OptTcpKeepaliveUseGarbage
        OptTagNisDomainName                 -> r OptNisDomainName
        OptTagNisServers                    -> r OptNisServers
        OptTagNtpServers                    -> r OptNtpServers
        OptTagVendorSpecific                -> r OptVendorSpecific
        OptTagNetBiosNameServers            -> r OptNetBiosNameServers
        OptTagNetBiosDistributionServers    -> r OptNetBiosDistributionServers
        OptTagNetBiosNodeType               -> r OptNetBiosNodeType
        OptTagNetBiosScope                  -> r OptNetBiosScope
        OptTagXWindowsFontServer            -> r OptXWindowsFontServer
        OptTagXWindowsDisplayManagers       -> r OptXWindowsDisplayManagers
        OptTagNisPlusDomain                 -> r OptNisPlusDomain
        OptTagNisPlusServers                -> r OptNisPlusServers
        OptTagSmtpServers                   -> r OptSmtpServers
        OptTagPopServers                    -> r OptPopServers
        OptTagNntpServers                   -> r OptNntpServers
        OptTagWwwServers                    -> r OptWwwServers
        OptTagFingerServers                 -> r OptFingerServers
        OptTagIrcServers                    -> r OptIrcServers
        OptTagStreetTalkServers             -> r OptStreetTalkServers
        OptTagStreetTalkDirectoryAssistanceServers -> r OptStreetTalkDirectoryAssistanceServers
        OptTagFQDN                          -> r OptFQDN
        OptTagRequestIPAddress              -> r OptRequestIPAddress
        OptTagIPAddressLeaseTime            -> r OptIPAddressLeaseTime
        OptTagOverload                      -> r OptOverload
        OptTagTftpServer                    -> r OptTftpServer
        OptTagBootfileName                  -> r OptBootfileName
        OptTagMessageType                   -> r OptMessageType
        OptTagServerIdentifier              -> r OptServerIdentifier
        OptTagParameterRequestList          -> r OptParameterRequestList
        OptTagErrorMessage                  -> r OptErrorMessage
        OptTagMaxDHCPMessageSize            -> r OptMaxDHCPMessageSize
        OptTagRenewalTime                   -> r OptRenewalTime
        OptTagRebindingTime                 -> r OptRebindingTime
        OptTagVendorClass                   -> r OptVendorClass
        OptTagClientIdentifier              -> r OptClientIdentifier
        OptTagNetWareDomainName             -> r OptNetWareDomainName
        OptTagNetWareInfo                   -> r OptNetWareInfo
        OptTagAutoconfiguration             -> r OptAutoconfiguration

putDhcp4Option :: Dhcp4Option -> Put
putDhcp4Option opt =
  let p tag val = do putAtom (KnownTag tag); putOption val in
  case opt of
    OptSubnetMask mask                  -> p OptTagSubnetMask mask
    OptTimeOffset offset                -> p OptTagTimeOffset offset
    OptRouters routers                  -> p OptTagRouters routers
    OptTimeServers servers              -> p OptTagTimeServers servers
    OptIEN116NameServers servers        -> p OptTagIEN116NameServers servers
    OptNameServers servers              -> p OptTagNameServers servers
    OptLogServers servers               -> p OptTagLogServers servers
    OptCookieServers servers            -> p OptTagCookieServers servers
    OptLPRServers servers               -> p OptTagLPRServers servers
    OptImpressServers servers           -> p OptTagImpressServers servers
    OptResourceLocationServers servers  -> p OptTagResourceLocationServers servers
    OptHostName hostname                -> p OptTagHostName hostname
    OptBootFileSize sz                  -> p OptTagBootFileSize sz
    OptMeritDumpFile file               -> p OptTagMeritDumpFile file
    OptDomainName domainname            -> p OptTagDomainName domainname
    OptSwapServer server                -> p OptTagSwapServer server
    OptRootPath path                    -> p OptTagRootPath path
    OptExtensionsPath path              -> p OptTagExtensionsPath path
    OptEnableIPForwarding enabled       -> p OptTagEnableIPForwarding enabled
    OptEnableNonLocalSourceRouting enab -> p OptTagEnableNonLocalSourceRouting enab
    OptPolicyFilters filters            -> p OptTagPolicyFilters filters
    OptMaximumDatagramReassemblySize n  -> p OptTagMaximumDatagramReassemblySize n
    OptDefaultTTL ttl                   -> p OptTagDefaultTTL ttl
    OptPathMTUAgingTimeout timeout      -> p OptTagPathMTUAgingTimeout timeout
    OptPathMTUPlateauTable mtus         -> p OptTagPathMTUPlateauTable mtus
    OptInterfaceMTU mtu                 -> p OptTagInterfaceMTU mtu
    OptAllSubnetsAreLocal arelocal      -> p OptTagAllSubnetsAreLocal arelocal
    OptBroadcastAddress addr            -> p OptTagBroadcastAddress addr
    OptPerformMaskDiscovery perform     -> p OptTagPerformMaskDiscovery perform
    OptShouldSupplyMasks should         -> p OptTagShouldSupplyMasks should
    OptShouldPerformRouterDiscovery b   -> p OptTagShouldPerformRouterDiscovery b
    OptRouterSolicitationAddress addr   -> p OptTagRouterSolicitationAddress addr
    OptStaticRoutes routes              -> p OptTagStaticRoutes routes
    OptShouldNegotiateArpTrailers b     -> p OptTagShouldNegotiateArpTrailers b
    OptArpCacheTimeout timeout          -> p OptTagArpCacheTimeout timeout
    OptUseRFC1042EthernetEncapsulation b-> p OptTagUseRFC1042EthernetEncapsulation b
    OptTcpDefaultTTL ttl                -> p OptTagTcpDefaultTTL ttl
    OptTcpKeepaliveInterval interval    -> p OptTagTcpKeepaliveInterval interval
    OptTcpKeepaliveUseGarbage use       -> p OptTagTcpKeepaliveUseGarbage use
    OptNisDomainName domainname         -> p OptTagNisDomainName domainname
    OptNisServers servers               -> p OptTagNisServers servers
    OptNtpServers servers               -> p OptTagNtpServers servers
    OptVendorSpecific bs                -> p OptTagVendorSpecific bs
    OptNetBiosNameServers servers       -> p OptTagNetBiosNameServers servers
    OptNetBiosDistributionServers srvs  -> p OptTagNetBiosDistributionServers srvs
    OptNetBiosNodeType node             -> p OptTagNetBiosNodeType node
    OptNetBiosScope scope               -> p OptTagNetBiosScope scope
    OptXWindowsFontServer servers       -> p OptTagXWindowsFontServer servers
    OptXWindowsDisplayManagers servers  -> p OptTagXWindowsDisplayManagers servers
    OptNisPlusDomain domain             -> p OptTagNisPlusDomain domain
    OptNisPlusServers servers           -> p OptTagNisPlusServers servers
    OptSmtpServers servers              -> p OptTagSmtpServers servers
    OptPopServers servers               -> p OptTagPopServers servers
    OptNntpServers servers              -> p OptTagNntpServers servers
    OptWwwServers servers               -> p OptTagWwwServers servers
    OptFingerServers servers            -> p OptTagFingerServers servers
    OptIrcServers servers               -> p OptTagIrcServers servers
    OptStreetTalkServers servers        -> p OptTagStreetTalkServers servers
    OptStreetTalkDirectoryAssistanceServers servers -> p OptTagStreetTalkDirectoryAssistanceServers servers
    OptFQDN fqdn                        -> p OptTagFQDN fqdn
    OptRequestIPAddress addr            -> p OptTagRequestIPAddress addr
    OptIPAddressLeaseTime time          -> p OptTagIPAddressLeaseTime time
    OptOverload overload                -> p OptTagOverload overload
    OptTftpServer server                -> p OptTagTftpServer server
    OptBootfileName filename            -> p OptTagBootfileName filename
    OptMessageType t                    -> p OptTagMessageType t
    OptServerIdentifier server          -> p OptTagServerIdentifier server
    OptParameterRequestList ps          -> p OptTagParameterRequestList ps
    OptErrorMessage msg                 -> p OptTagErrorMessage msg
    OptMaxDHCPMessageSize maxsz         -> p OptTagMaxDHCPMessageSize maxsz
    OptRenewalTime time                 -> p OptTagRenewalTime time
    OptRebindingTime time               -> p OptTagRebindingTime time
    OptVendorClass str                  -> p OptTagVendorClass str
    OptClientIdentifier client          -> p OptTagClientIdentifier client
    OptNetWareDomainName name           -> p OptTagNetWareDomainName name
    OptNetWareInfo info                 -> p OptTagNetWareInfo info
    OptAutoconfiguration autoconf       -> p OptTagAutoconfiguration autoconf

-----------------------------------------------------------------------
-- Message Type type and operations -----------------------------------
-----------------------------------------------------------------------

data Dhcp4MessageType
  = Dhcp4Discover
  | Dhcp4Offer
  | Dhcp4Request
  | Dhcp4Decline
  | Dhcp4Ack
  | Dhcp4Nak
  | Dhcp4Release
  | Dhcp4Inform
    deriving (Eq,Show)

instance Option Dhcp4MessageType where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption

instance CodecAtom Dhcp4MessageType where
  getAtom = do
    b <- getAtom
    case b :: Word8 of
      1 -> return Dhcp4Discover
      2 -> return Dhcp4Offer
      3 -> return Dhcp4Request
      4 -> return Dhcp4Decline
      5 -> return Dhcp4Ack
      6 -> return Dhcp4Nak
      7 -> return Dhcp4Release
      8 -> return Dhcp4Inform
      _ -> fail ("Unknown DHCP Message Type 0x" ++ showHex b "")

  putAtom t = putAtom $ case t of
    Dhcp4Discover -> 1 :: Word8
    Dhcp4Offer    -> 2
    Dhcp4Request  -> 3
    Dhcp4Decline  -> 4
    Dhcp4Ack      -> 5
    Dhcp4Nak      -> 6
    Dhcp4Release  -> 7
    Dhcp4Inform   -> 8

  atomSize _ = 1

-----------------------------------------------------------------------
-- Control tag type and operations ------------------------------------
-----------------------------------------------------------------------

data ControlTag
  = ControlPad
  | ControlEnd
  deriving (Eq, Show)

putControlOption :: ControlTag -> Put
putControlOption opt = case opt of
    ControlPad -> putAtom (KnownTag OptTagPad)
    ControlEnd -> putAtom (KnownTag OptTagEnd)

-----------------------------------------------------------------------
-- Option tag type and operations -------------------------------------
-----------------------------------------------------------------------

data Dhcp4OptionTag
  = OptTagPad
  | OptTagEnd
  | OptTagSubnetMask
  | OptTagTimeOffset
  | OptTagRouters
  | OptTagTimeServers
  | OptTagIEN116NameServers
  | OptTagNameServers
  | OptTagLogServers
  | OptTagCookieServers
  | OptTagLPRServers
  | OptTagImpressServers
  | OptTagResourceLocationServers
  | OptTagHostName
  | OptTagBootFileSize
  | OptTagMeritDumpFile
  | OptTagDomainName
  | OptTagSwapServer
  | OptTagRootPath
  | OptTagExtensionsPath
  | OptTagEnableIPForwarding
  | OptTagEnableNonLocalSourceRouting
  | OptTagPolicyFilters
  | OptTagMaximumDatagramReassemblySize
  | OptTagDefaultTTL
  | OptTagPathMTUAgingTimeout
  | OptTagPathMTUPlateauTable
  | OptTagInterfaceMTU
  | OptTagAllSubnetsAreLocal
  | OptTagBroadcastAddress
  | OptTagPerformMaskDiscovery
  | OptTagShouldSupplyMasks
  | OptTagShouldPerformRouterDiscovery
  | OptTagRouterSolicitationAddress
  | OptTagStaticRoutes
  | OptTagShouldNegotiateArpTrailers
  | OptTagArpCacheTimeout
  | OptTagUseRFC1042EthernetEncapsulation
  | OptTagTcpDefaultTTL
  | OptTagTcpKeepaliveInterval
  | OptTagTcpKeepaliveUseGarbage
  | OptTagNisDomainName
  | OptTagNisServers
  | OptTagNtpServers
  | OptTagVendorSpecific
  | OptTagNetBiosNameServers
  | OptTagNetBiosDistributionServers
  | OptTagNetBiosNodeType
  | OptTagNetBiosScope
  | OptTagXWindowsFontServer
  | OptTagXWindowsDisplayManagers
  | OptTagNisPlusDomain
  | OptTagNisPlusServers
  | OptTagSmtpServers
  | OptTagPopServers
  | OptTagNntpServers
  | OptTagWwwServers
  | OptTagFingerServers
  | OptTagIrcServers
  | OptTagStreetTalkServers
  | OptTagStreetTalkDirectoryAssistanceServers
  | OptTagFQDN
  | OptTagRequestIPAddress
  | OptTagIPAddressLeaseTime
  | OptTagOverload
  | OptTagTftpServer
  | OptTagBootfileName
  | OptTagMessageType
  | OptTagServerIdentifier
  | OptTagParameterRequestList
  | OptTagErrorMessage
  | OptTagMaxDHCPMessageSize
  | OptTagRenewalTime
  | OptTagRebindingTime
  | OptTagVendorClass
  | OptTagClientIdentifier
  | OptTagNetWareDomainName
  | OptTagNetWareInfo
  | OptTagAutoconfiguration
  deriving (Show,Eq)

data OptionTagOrError = UnknownTag Word8 | KnownTag Dhcp4OptionTag
  deriving (Show,Eq)

getOptionTag :: Get OptionTagOrError
getOptionTag = f =<< getWord8
  where
  r     = return . KnownTag
  f 0   = r OptTagPad
  f 1   = r OptTagSubnetMask
  f 2   = r OptTagTimeOffset
  f 3   = r OptTagRouters
  f 4   = r OptTagTimeServers
  f 5   = r OptTagIEN116NameServers
  f 6   = r OptTagNameServers
  f 7   = r OptTagLogServers
  f 8   = r OptTagCookieServers
  f 9   = r OptTagLPRServers
  f 10  = r OptTagImpressServers
  f 11  = r OptTagResourceLocationServers
  f 12  = r OptTagHostName
  f 13  = r OptTagBootFileSize
  f 14  = r OptTagMeritDumpFile
  f 15  = r OptTagDomainName
  f 16  = r OptTagSwapServer
  f 17  = r OptTagRootPath
  f 18  = r OptTagExtensionsPath
  f 19  = r OptTagEnableIPForwarding
  f 20  = r OptTagEnableNonLocalSourceRouting
  f 21  = r OptTagPolicyFilters
  f 22  = r OptTagMaximumDatagramReassemblySize
  f 23  = r OptTagDefaultTTL
  f 24  = r OptTagPathMTUAgingTimeout
  f 25  = r OptTagPathMTUPlateauTable
  f 26  = r OptTagInterfaceMTU
  f 27  = r OptTagAllSubnetsAreLocal
  f 28  = r OptTagBroadcastAddress
  f 29  = r OptTagPerformMaskDiscovery
  f 30  = r OptTagShouldSupplyMasks
  f 31  = r OptTagShouldPerformRouterDiscovery
  f 32  = r OptTagRouterSolicitationAddress
  f 33  = r OptTagStaticRoutes
  f 34  = r OptTagShouldNegotiateArpTrailers
  f 35  = r OptTagArpCacheTimeout
  f 36  = r OptTagUseRFC1042EthernetEncapsulation
  f 37  = r OptTagTcpDefaultTTL
  f 38  = r OptTagTcpKeepaliveInterval
  f 39  = r OptTagTcpKeepaliveUseGarbage
  f 40  = r OptTagNisDomainName
  f 41  = r OptTagNisServers
  f 42  = r OptTagNtpServers
  f 43  = r OptTagVendorSpecific
  f 44  = r OptTagNetBiosNameServers
  f 45  = r OptTagNetBiosDistributionServers
  f 46  = r OptTagNetBiosNodeType
  f 47  = r OptTagNetBiosScope
  f 48  = r OptTagXWindowsFontServer
  f 49  = r OptTagXWindowsDisplayManagers
  f 50  = r OptTagRequestIPAddress
  f 51  = r OptTagIPAddressLeaseTime
  f 52  = r OptTagOverload
  f 53  = r OptTagMessageType
  f 54  = r OptTagServerIdentifier
  f 55  = r OptTagParameterRequestList
  f 56  = r OptTagErrorMessage
  f 57  = r OptTagMaxDHCPMessageSize
  f 58  = r OptTagRenewalTime
  f 59  = r OptTagRebindingTime
  f 60  = r OptTagVendorClass
  f 61  = r OptTagClientIdentifier
  f 62  = r OptTagNetWareDomainName
  f 63  = r OptTagNetWareInfo
  f 64  = r OptTagNisPlusDomain
  f 65  = r OptTagNisPlusServers
  f 66  = r OptTagTftpServer
  f 67  = r OptTagBootfileName
  f 69  = r OptTagSmtpServers
  f 70  = r OptTagPopServers
  f 71  = r OptTagNntpServers
  f 72  = r OptTagWwwServers
  f 73  = r OptTagFingerServers
  f 74  = r OptTagIrcServers
  f 75  = r OptTagStreetTalkServers
  f 76  = r OptTagStreetTalkDirectoryAssistanceServers
  f 81  = r OptTagFQDN
  f 116 = r OptTagAutoconfiguration
  f 255 = r OptTagEnd
  f t   = return (UnknownTag t)

putOptionTag :: OptionTagOrError -> Put
putOptionTag (UnknownTag t) = putAtom t
putOptionTag (KnownTag t) = putAtom (f t)
  where
  f :: Dhcp4OptionTag -> Word8
  f OptTagPad                                   = 0
  f OptTagEnd                                   = 255
  f OptTagSubnetMask                            = 1
  f OptTagTimeOffset                            = 2
  f OptTagRouters                               = 3
  f OptTagTimeServers                           = 4
  f OptTagIEN116NameServers                     = 5
  f OptTagNameServers                           = 6
  f OptTagLogServers                            = 7
  f OptTagCookieServers                         = 8
  f OptTagLPRServers                            = 9
  f OptTagImpressServers                        = 10
  f OptTagResourceLocationServers               = 11
  f OptTagHostName                              = 12
  f OptTagBootFileSize                          = 13
  f OptTagMeritDumpFile                         = 14
  f OptTagDomainName                            = 15
  f OptTagSwapServer                            = 16
  f OptTagRootPath                              = 17
  f OptTagExtensionsPath                        = 18
  f OptTagEnableIPForwarding                    = 19
  f OptTagEnableNonLocalSourceRouting           = 20
  f OptTagPolicyFilters                         = 21
  f OptTagMaximumDatagramReassemblySize         = 22
  f OptTagDefaultTTL                            = 23
  f OptTagPathMTUAgingTimeout                   = 24
  f OptTagPathMTUPlateauTable                   = 25
  f OptTagInterfaceMTU                          = 26
  f OptTagAllSubnetsAreLocal                    = 27
  f OptTagBroadcastAddress                      = 28
  f OptTagPerformMaskDiscovery                  = 29
  f OptTagShouldSupplyMasks                     = 30
  f OptTagShouldPerformRouterDiscovery          = 31
  f OptTagRouterSolicitationAddress             = 32
  f OptTagStaticRoutes                          = 33
  f OptTagShouldNegotiateArpTrailers            = 34
  f OptTagArpCacheTimeout                       = 35
  f OptTagUseRFC1042EthernetEncapsulation       = 36
  f OptTagTcpDefaultTTL                         = 37
  f OptTagTcpKeepaliveInterval                  = 38
  f OptTagTcpKeepaliveUseGarbage                = 39
  f OptTagNisDomainName                         = 40
  f OptTagNisServers                            = 41
  f OptTagNtpServers                            = 42
  f OptTagVendorSpecific                        = 43
  f OptTagNetBiosNameServers                    = 44
  f OptTagNetBiosDistributionServers            = 45
  f OptTagNetBiosNodeType                       = 46
  f OptTagNetBiosScope                          = 47
  f OptTagXWindowsFontServer                    = 48
  f OptTagXWindowsDisplayManagers               = 49
  f OptTagRequestIPAddress                      = 50
  f OptTagIPAddressLeaseTime                    = 51
  f OptTagOverload                              = 52
  f OptTagMessageType                           = 53
  f OptTagServerIdentifier                      = 54
  f OptTagParameterRequestList                  = 55
  f OptTagErrorMessage                          = 56
  f OptTagMaxDHCPMessageSize                    = 57
  f OptTagRenewalTime                           = 58
  f OptTagRebindingTime                         = 59
  f OptTagVendorClass                           = 60
  f OptTagClientIdentifier                      = 61
  f OptTagNetWareDomainName                     = 62
  f OptTagNetWareInfo                           = 63
  f OptTagNisPlusDomain                         = 64
  f OptTagNisPlusServers                        = 65
  f OptTagTftpServer                            = 66
  f OptTagBootfileName                          = 67
  f OptTagSmtpServers                           = 69
  f OptTagPopServers                            = 70
  f OptTagNntpServers                           = 71
  f OptTagWwwServers                            = 72
  f OptTagFingerServers                         = 73
  f OptTagIrcServers                            = 74
  f OptTagStreetTalkServers                     = 75
  f OptTagStreetTalkDirectoryAssistanceServers  = 76
  f OptTagFQDN                                  = 81
  f OptTagAutoconfiguration                     = 116

-----------------------------------------------------------------------
-- NetBIOS node type and operations -----------------------------------
-----------------------------------------------------------------------

data NetBiosNodeType
  = BNode
  | PNode
  | MNode
  | HNode
  deriving (Show,Eq)

instance Option NetBiosNodeType where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption

instance CodecAtom NetBiosNodeType where
  getAtom = do
    b <- getAtom
    case b :: Word8 of
      0x1 -> return BNode
      0x2 -> return PNode
      0x4 -> return MNode
      0x8 -> return HNode
      _   -> fail "Unknown NetBIOS node type"

  putAtom t = putAtom $ case t of
    BNode -> 0x1 :: Word8
    PNode -> 0x2
    MNode -> 0x4
    HNode -> 0x8

  atomSize _ = 1

-----------------------------------------------------------------------
-- Overload option type and operations --------------------------------
-----------------------------------------------------------------------

data OverloadOption
  = UsedFileField
  | UsedSNameField
  | UsedBothFields
  deriving (Show, Eq)

instance Option OverloadOption where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption

instance CodecAtom OverloadOption where
  getAtom = do b <- getAtom
               case b :: Word8 of
                  1 -> return UsedFileField
                  2 -> return UsedSNameField
                  3 -> return UsedBothFields
                  _ -> fail ("Bad overload value 0x" ++ showHex b "")
  putAtom t = putAtom $ case t of
    UsedFileField  -> 1 :: Word8
    UsedSNameField -> 2
    UsedBothFields -> 3

  atomSize _ = atomSize (undefined :: Word8)

-----------------------------------------------------------------------
-- Options list operations --------------------------------------------
-----------------------------------------------------------------------

getDhcp4Options :: ByteString -> ByteString
                -> Get (String, String, [Dhcp4Option])
getDhcp4Options sname file = do
  MagicCookie <- getAtom
  options0 <- remainingAsOptions
  case lookupOverload options0 of
    Nothing -> return (nullTerminated sname, nullTerminated file, options0)

    Just UsedFileField -> do
      options1 <- localParse file remainingAsOptions
      let options = options0 ++ options1
          NVTAsciiString fileString
             = fromMaybe (NVTAsciiString "") (lookupFile options)
      return (nullTerminated sname, fileString, options)

    Just UsedSNameField -> do
      options1 <- localParse sname remainingAsOptions
      let options = options0 ++ options1
          NVTAsciiString snameString
            = fromMaybe (NVTAsciiString "") (lookupSname options)
      return (snameString, nullTerminated file, options)

    Just UsedBothFields -> do

      -- The file field MUST be interpreted for options before the sname field.
      -- RFC 2131, Section 4.1, Page 24
      options1 <- localParse file  remainingAsOptions
      options2 <- localParse sname remainingAsOptions
      let options = options0 ++ options1 ++ options2
          NVTAsciiString snameString
            = fromMaybe (NVTAsciiString "") (lookupSname options)
          NVTAsciiString fileString
            = fromMaybe (NVTAsciiString "") (lookupFile options)
      return (snameString, fileString, options)

  where
  remainingAsOptions = scrubControls =<< repeatedly getDhcp4Option

  localParse bs m = case runGet m bs of
    Right x -> return x
    Left err -> fail err


putDhcp4Options :: [Dhcp4Option] -> Put
putDhcp4Options opts =
  do putAtom MagicCookie
     traverse_ putDhcp4Option opts
     putControlOption ControlEnd

scrubControls :: (A.Applicative m, Monad m)
              => [Either ControlTag Dhcp4Option] -> m [Dhcp4Option]

scrubControls [] =
     fail "No END option found"

scrubControls (Left ControlPad : xs) =
     scrubControls xs

scrubControls (Left ControlEnd : xs) =
  do traverse_ eatPad xs
     return []

scrubControls (Right o : xs) =
  do os <- scrubControls xs
     return (o:os)

-- | 'eatPad' fails on any non 'ControlPad' option with an error message.
eatPad :: Monad m => Either ControlTag Dhcp4Option -> m ()
eatPad (Left ControlPad) = return ()
eatPad _                 = fail "Unexpected option after END option"

replicateA :: A.Applicative f => Int -> f a -> f [a]
replicateA n f = T.sequenceA (replicate n f)

repeatedly :: Get a -> Get [a]
repeatedly m = go []
  where
  go acc =
    do done <- isEmpty
       if done then return (reverse acc)
               else do a <- m
                       go (a:acc)

nullTerminated :: ByteString -> String
nullTerminated = takeWhile (/= '\NUL') . BS8.unpack

lookupOverload :: [Dhcp4Option] -> Maybe OverloadOption
lookupOverload = foldr f Nothing
  where f (OptOverload o) _ = Just o
        f _               a = a

lookupFile :: [Dhcp4Option] -> Maybe NVTAsciiString
lookupFile = foldr f Nothing
  where f (OptBootfileName fn) _ = Just fn
        f _                    a = a

lookupSname :: [Dhcp4Option] -> Maybe NVTAsciiString
lookupSname = foldr f Nothing
  where f (OptTftpServer n) _ = Just n
        f _                 a = a

lookupParams :: [Dhcp4Option] -> Maybe [OptionTagOrError]
lookupParams = foldr f Nothing
  where f (OptParameterRequestList n) _ = Just n
        f _                           a = a

lookupMessageType :: [Dhcp4Option] -> Maybe Dhcp4MessageType
lookupMessageType = foldr f Nothing
  where f (OptMessageType n) _ = Just n
        f _                  a = a

lookupRequestAddr :: [Dhcp4Option] -> Maybe IP4
lookupRequestAddr = foldr f Nothing
  where f (OptRequestIPAddress n) _ = Just n
        f _                       a = a

lookupLeaseTime :: [Dhcp4Option] -> Maybe Word32
lookupLeaseTime = foldr f Nothing
  where f (OptIPAddressLeaseTime t) _ = Just t
        f _                         a = a

-----------------------------------------------------------------------
-- Protected parser and unparser monad --------------------------------
-----------------------------------------------------------------------

class Option a where
  getOption :: Get a
  putOption :: a -> Put

instance CodecAtom a => Option [a] where
  getOption = do
    let (n, m) = getRecord
    len <- getLen
    let (count, remainder) = divMod len n
    unless (remainder == 0) (fail ("Length was not a multiple of " ++ show n))
    unless (count > 0) (fail "Minimum length not met")
    replicateA count $ label "List of fixed-length values" $ isolate n m
  putOption xs = do putLen (atomSize (head xs) * length xs)
                    traverse_ putAtom xs

instance (CodecAtom a, CodecAtom b) => Option (a,b) where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option Bool where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option Word8 where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option Word16 where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option Word32 where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option IP4 where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option SubnetMask where
  getOption = defaultFixedGetOption
  putOption = defaultFixedPutOption
instance Option ByteString where
  getOption = do len <- getLen
                 getByteString len
  putOption bs = do putLen (BS.length bs)
                    putByteString bs

defaultFixedGetOption :: CodecAtom a => Get a
defaultFixedGetOption = fixedLen n m
  where (n,m) = getRecord

defaultFixedPutOption :: CodecAtom a => a -> Put
defaultFixedPutOption x = do
  putLen (atomSize x)
  putAtom x

fixedLen :: Int -> Get a -> Get a
fixedLen expectedLen m = do
  len <- getLen
  unless (len == expectedLen) (fail "Bad length on \"fixed-length\" option.")
  label "Fixed length field" (isolate expectedLen m)

getRecord :: CodecAtom a => (Int, Get a)
getRecord = (atomSize undef, m)
  where
  (undef, m) = (undefined, getAtom) :: CodecAtom a => (a, Get a)


instance CodecAtom OptionTagOrError where
  getAtom       = getOptionTag
  putAtom x     = putOptionTag x
  atomSize _    = 1

newtype NVTAsciiString = NVTAsciiString String
  deriving (Eq, Show)

instance Option NVTAsciiString where
  getOption = do len <- getLen
                 bs  <- getByteString len
                 return (NVTAsciiString (nullTerminated bs))
  putOption (NVTAsciiString str) = do
    putLen (length str)
    putByteString (BS8.pack str)

getLen :: Get Int
getLen = fromIntegral `fmap` getWord8

putLen :: Int -> Put
putLen n = putWord8 (fromIntegral n)
