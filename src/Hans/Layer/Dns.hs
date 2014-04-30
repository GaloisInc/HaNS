{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hans.Layer.Dns (
    DnsHandle
  , runDnsLayer
  , DnsException

  , addNameServer
  , removeNameServer

  , HostName
  , HostEntry(..)
  , getHostByName
  , getHostByAddr
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Udp as Udp
import Hans.Message.Dns
import Hans.Message.Udp

import Control.Concurrent ( forkIO, MVar, newEmptyMVar, takeMVar, putMVar )
import Control.Monad ( mzero, guard, when )
import Data.Bits ( shiftR, (.&.), (.|.) )
import Data.Foldable ( foldl' )
import Data.List ( intercalate )
import Data.String ( fromString )
import Data.Typeable ( Typeable )
import Data.Word ( Word16 )
import MonadLib ( get, set )
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map


-- External Interface ----------------------------------------------------------

type DnsHandle = Channel (Dns ())

runDnsLayer :: DnsHandle -> UdpHandle -> IO ()
runDnsLayer h udp =
  do _ <- forkIO (loopLayer "dns" (emptyDnsState h udp) (receive h) id)
     return ()

data DnsException = NoNameServers
                    -- ^ No name servers have been configured
                  | OutOfServers
                    -- ^ Ran out of name servers to try
                  | DoesNotExist
                    -- ^ Unable to find any information about the host
                  | DnsRequestFailed
                    deriving (Show,Typeable)

instance X.Exception DnsException

addNameServer :: DnsHandle -> IP4 -> IO ()
addNameServer h addr =
  send h $ do state <- get
              set $! state { dnsNameServers = addr : dnsNameServers state }

removeNameServer :: DnsHandle -> IP4 -> IO ()
removeNameServer h addr =
  send h $ do state <- get
              set $! state
                { dnsNameServers = filter (/= addr) (dnsNameServers state) }

type HostName = String

data HostEntry = HostEntry { hostName      :: HostName
                           , hostAliases   :: [HostName]
                           , hostAddresses :: [IP4]
                           } deriving (Show)

getHostByName :: DnsHandle -> HostName -> IO HostEntry
getHostByName h host =
  do res <- newEmptyMVar
     send h (getHostEntry res (FromHost host))
     e <- takeMVar res
     case e of
       Right he -> return he
       Left err -> X.throwIO err


getHostByAddr :: DnsHandle -> IP4 -> IO HostEntry
getHostByAddr h addr =
  do res <- newEmptyMVar
     send h (getHostEntry res (FromIP4 addr))
     e <- takeMVar res
     case e of
       Right he -> return he
       Left err -> X.throwIO err


-- Handlers --------------------------------------------------------------------

type Dns = Layer DnsState

data DnsState = DnsState { dnsSelf        :: DnsHandle
                         , dnsUdpHandle   :: UdpHandle
                         , dnsNameServers :: [IP4]
                         , dnsReqId       :: !Word16
                         , dnsQueries     :: Map.Map Word16 DnsQuery
                         }

emptyDnsState :: DnsHandle -> UdpHandle -> DnsState
emptyDnsState h udp = DnsState { dnsSelf        = h
                               , dnsUdpHandle   = udp
                               , dnsNameServers = []
                               , dnsReqId       = 1
                               , dnsQueries     = Map.empty }

-- LFSR: x^16 + x^14 + x^13 + x^11 + 1
--
-- 0xB400 ~ bit 15 .|. bit 13 .|. bit 12 .|. bit 10
stepReqId :: Word16 -> Word16
stepReqId w = (w `shiftR` 1) .|. (negate (w .&. 0x1) .&. 0xB400)

registerRequest :: (Word16 -> DnsQuery) -> Dns Word16
registerRequest mk =
  do state <- get
     let reqId = dnsReqId state
     set state { dnsReqId   = stepReqId reqId
               , dnsQueries = Map.insert reqId (mk reqId) (dnsQueries state)
               }
     return reqId

updateRequest :: Word16 -> DnsQuery -> Dns ()
updateRequest reqId query =
  do state <- get
     set state { dnsQueries = Map.insert reqId query (dnsQueries state) }

lookupRequest :: Word16 -> Dns DnsQuery
lookupRequest reqId =
  do DnsState { .. } <- get
     case Map.lookup reqId dnsQueries of
       Just query -> return query
       Nothing    -> mzero

removeRequest :: Word16 -> Dns ()
removeRequest reqId =
  do state <- get
     set state { dnsQueries = Map.delete reqId (dnsQueries state) }


data Source = FromHost HostName
            | FromIP4 IP4
              deriving (Show)

sourceQType :: Source -> [QType]
sourceQType FromHost{} = [QType A]
sourceQType FromIP4{}  = [QType PTR]

sourceHost :: Source -> Name
sourceHost (FromHost h)            = toLabels h
sourceHost (FromIP4 (IP4 a b c d)) = let byte w = fromString (show w)
                                      in map byte [a,b,c,d] ++ ["in-addr","arpa"]

toLabels :: String -> Name
toLabels str = case break (== '.') str of
  (as,_:bs) -> fromString as : toLabels bs
  (as,_)    -> [fromString as]

getHostEntry :: DnsResult -> Source -> Dns ()
getHostEntry res src =
  do DnsState { .. } <- get

     -- make sure that there are name servers to work with
     when (null dnsNameServers) $
       do output (putError res NoNameServers)
          mzero

     -- register a upd handler on a fresh port, and query the name servers in
     -- order
     output $
       do port <- addUdpHandlerAnyPort dnsUdpHandle (serverResponse dnsSelf src)
          send dnsSelf (createRequest res dnsNameServers src port)


-- | Create the query packet, and register the request with the DNS layer.
-- Then, send a request to the first name server.
createRequest :: DnsResult -> [IP4] -> Source -> UdpPort -> Dns ()
createRequest res nss src port =
  do DnsState { .. } <- get
     reqId <- registerRequest (mkDnsQuery res nss port src)
     sendRequest reqId


-- | Send a request to the next name server in the queue.
sendRequest :: Word16 -> Dns ()
sendRequest reqId =
  do query <- lookupRequest reqId

     case qServers query of

       n:rest -> do updateRequest reqId query { qServers    = rest
                                              , qLastServer = Just n
                                              }
                    sendQuery n (qUdpPort query) (qRequest query)

       -- out of servers to try
       [] -> do removeRequest reqId
                output (putError (qResult query) OutOfServers)


-- | Handle the response from the server.
handleResponse :: Source -> IP4 -> UdpPort -> S.ByteString -> Dns ()
handleResponse src srcIp srcPort bytes =
  do guard (srcPort == 53)

     DNSPacket { .. } <- liftRight (parseDNSPacket bytes)
     let DNSHeader { .. } = dnsHeader
     req <- lookupRequest dnsId

     -- require that the last name server we sent to was the one that responded,
     -- and that it responded with a response, not a request.
     guard (Just srcIp == qLastServer req && not dnsQuery)

     if dnsRC == RespNoError
        then output (putResult (qResult req) (parseHostEntry src dnsAnswers))
        else output (putError  (qResult req) DnsRequestFailed)

     removeRequest dnsId
     DnsState { .. } <- get
     output (removeUdpHandler dnsUdpHandle (qUdpPort req))

parseHostEntry :: Source -> [RR] -> HostEntry
parseHostEntry (FromHost host) = parseAddr host
parseHostEntry (FromIP4 addr)  = parsePtr addr

-- | Parse the A and CNAME parts out of a response.
parseAddr :: HostName -> [RR] -> HostEntry
parseAddr host = foldl' processAnswer emptyHostEntry
  where

  emptyHostEntry = HostEntry { hostName      = host
                             , hostAliases   = []
                             , hostAddresses = [] }

  processAnswer he RR { .. } = case rrRData of
    RDA ip     -> he { hostAddresses = ip : hostAddresses he }
    RDCNAME ns -> he { hostName      = intercalate "." (map C8.unpack ns)
                     , hostAliases   = hostName he : hostAliases he }
    _          -> he

parsePtr :: IP4 -> [RR] -> HostEntry
parsePtr addr = foldl' processAnswer emptyHostEntry
  where
  emptyHostEntry = HostEntry { hostName      = ""
                             , hostAliases   = []
                             , hostAddresses = [addr] }

  processAnswer he RR { .. } = case rrRData of
    RDPTR name -> he { hostName = intercalate "." (map C8.unpack name) }
    _          -> he


-- Query Management ------------------------------------------------------------

type DnsResult = MVar (Either DnsException HostEntry)

putResult :: DnsResult -> HostEntry -> IO ()
putResult var he = putMVar var (Right he)

putError :: DnsResult -> DnsException -> IO ()
putError var err = putMVar var (Left err)

data DnsQuery = DnsQuery { qResult     :: DnsResult
                           -- ^ The handle back to the thread waiting fo the
                           -- HostEntry
                         , qUdpPort    :: !UdpPort
                           -- ^ The port this request is receiving on 
                         , qRequest    :: L.ByteString
                           -- ^ The packet to send
                         , qServers    :: [IP4]
                           -- ^ Name servers left to try
                         , qLastServer :: Maybe IP4
                         }

mkDnsQuery :: DnsResult -> [IP4] -> UdpPort -> Source -> Word16 -> DnsQuery
mkDnsQuery res nss port src reqId =
  DnsQuery { qResult     = res
           , qUdpPort    = port
           , qRequest    = renderDNSPacket (mkDNSPacket host qs reqId)
           , qServers    = nss
           , qLastServer = Nothing
           }
  where
  host = sourceHost src
  qs   = sourceQType src


mkDNSPacket :: Name -> [QType] -> Word16 -> DNSPacket
mkDNSPacket name qs reqId =
  DNSPacket { dnsHeader            = hdr
            , dnsQuestions         = [ mkQuery q | q <- qs ]
            , dnsAnswers           = []
            , dnsAuthorityRecords  = []
            , dnsAdditionalRecords = []
            }
  where
  hdr = DNSHeader { dnsId     = reqId
                  , dnsQuery  = True
                  , dnsOpCode = OpQuery
                  , dnsAA     = False
                  , dnsTC     = False
                  , dnsRD     = True
                  , dnsRA     = False
                  , dnsRC     = RespNoError
                  }

  mkQuery qty = Query { qName  = name
                      , qType  = qty
                      , qClass = QClass IN
                      }


-- UDP Interaction -------------------------------------------------------------

-- | Send a UDP query to the server given
sendQuery :: IP4 -> UdpPort -> L.ByteString -> Dns ()
sendQuery nameServer sp bytes =
  do DnsState { .. } <- get
     output (sendUdp dnsUdpHandle nameServer (Just sp) 53 bytes)

-- | Queue the packet into the DNS layer for processing.
serverResponse :: DnsHandle -> Source -> UdpPort -> Udp.Handler
serverResponse dns src _ srcIp srcPort bytes =
  send dns (handleResponse src srcIp srcPort bytes)
