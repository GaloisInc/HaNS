{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Hans.Dns where

import Hans.Addr
import Hans.Config
import Hans.Dns.Packet
import Hans.Lens
import Hans.Serialize (runPutPacket)
import Hans.Socket
import Hans.Types

import           Control.Exception
import           Control.Monad (when)
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Serialize (runGetLazy)
import           Data.Typeable (Typeable)
import           Data.Word (Word16)
import           System.Timeout (timeout)


type HostName = S8.ByteString

data HostEntry = HostEntry { hostName      :: HostName
                           , hostAliases   :: [HostName]
                           , hostAddresses :: [IP4]
                           } deriving (Show)

parseHostEntry :: Source -> [RR] -> HostEntry
parseHostEntry (FromHost host)  = parseAddr host
parseHostEntry (FromAddr4 addr) = parsePtr addr

-- | Parse the A and CNAME parts out of a response.
parseAddr :: HostName -> [RR] -> HostEntry
parseAddr host = F.foldl' processAnswer emptyHostEntry
  where

  emptyHostEntry = HostEntry { hostName      = host
                             , hostAliases   = []
                             , hostAddresses = [] }

  processAnswer he RR { .. } = case rrRData of
    RDA ip     -> he { hostAddresses = ip : hostAddresses he }
    RDCNAME ns -> he { hostName      = S8.intercalate "." ns
                     , hostAliases   = hostName he : hostAliases he }
    _          -> he

parsePtr :: IP4 -> [RR] -> HostEntry
parsePtr addr = F.foldl' processAnswer emptyHostEntry
  where
  emptyHostEntry = HostEntry { hostName      = ""
                             , hostAliases   = []
                             , hostAddresses = [addr] }

  processAnswer he RR { .. } = case rrRData of
    RDPTR name -> he { hostName = S8.intercalate "." name }
    _          -> he


data DnsException = NoNameServers
                    deriving (Show,Typeable)

instance Exception DnsException


getHostByName :: HasNetworkStack ns => ns -> HostName -> IO (Maybe HostEntry)
getHostByName ns host = sendRequest ns (FromHost host)

sendRequest :: HasNetworkStack ns => ns -> Source -> IO (Maybe HostEntry)
sendRequest ns src =
  do nameServers <- getNameServers4 ns

     when (null nameServers) (throwIO NoNameServers)

     bracket (newUdpSocket ns defaultSocketConfig Nothing WildcardIP4 Nothing)
             sClose $ \ sock ->
       do let req = runPutPacket 1450 1450 L.empty (putDNSPacket (mkPacket src 0))

          mbResp <- queryServers4 sock req nameServers
          case mbResp of
            Just DNSPacket { .. }
              | DNSHeader { .. } <- dnsHeader
              , dnsRC == RespNoError ->
                return (Just (parseHostEntry src dnsAnswers))
            _ ->
              return Nothing


queryServers4 :: UdpSocket IP4 -> L.ByteString -> [IP4] -> IO (Maybe DNSPacket)
queryServers4 sock req = go
  where
  go (addr:addrs) =
    do sendto sock addr 53 req
       mbRes <- timeout (cfgDnsResolveTimeout (view config (view networkStack sock)))
                    (recvfrom sock)

       -- require that the server we sent a request to is the one that responded
       case mbRes of
         Just (_,srcIp,srcPort,bytes)
           | srcIp == addr, srcPort == 53 ->
             case runGetLazy getDNSPacket bytes of
               Right res -> return (Just res)
               Left _    -> return Nothing

         _ -> go addrs

  go [] = return Nothing


data Source = FromHost HostName
            | FromAddr4 IP4
              deriving (Show)

sourceHost :: Source -> Name
sourceHost (FromHost host) = toLabels host
sourceHost (FromAddr4 ip4) = let (a,b,c,d)  = unpackIP4 ip4
                                 showByte x = S8.pack (show x)
                              in map showByte [d,c,b,a] ++ ["in-addr","arpa"]

toLabels :: HostName -> Name
toLabels str =
  case S8.break (== '.') str of
    (as,bs) | S8.null bs -> [as]
            | otherwise  -> as : toLabels (S8.tail bs)

sourceQType :: Source -> [QType]
sourceQType FromHost{}  = [QType A]
sourceQType FromAddr4{} = [QType PTR]

mkPacket :: Source -> Word16 -> DNSPacket
mkPacket src dnsId =
  DNSPacket { dnsHeader            = hdr
            , dnsQuestions         = [ mkQuery q | q <- sourceQType src ]
            , dnsAnswers           = []
            , dnsAuthorityRecords  = []
            , dnsAdditionalRecords = []
            }
  where
  n = sourceHost src

  hdr =
    DNSHeader { dnsQuery  = True
              , dnsOpCode = OpQuery
              , dnsAA     = False
              , dnsTC     = False
              , dnsRD     = True
              , dnsRA     = False
              , dnsRC     = RespNoError
              , .. }

  mkQuery qType =
    Query { qName = n
          , qClass = QClass IN
          , .. }
