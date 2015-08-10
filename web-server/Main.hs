{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
-- Copyright 2014 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
import Data.Time
import Data.Version
import Data.Word
import Hans.Address.Mac
import Hans.Address.IP4
import Hans.Device.Tap
import Hans.DhcpClient
import Hans.Layer.Dns(DnsException)
import Hans.NetworkStack hiding (close)
import qualified Hans.NetworkStack as Hans
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Stream
import System.Exit
import System.Info
import Text.Blaze.Html5 as H hiding (map, main)
import Text.Blaze.Html5.Attributes(href)
import Text.Blaze.Html.Renderer.String

#if !MIN_VERSION_base(4,8,0)
import           System.Locale
#endif

instance Stream Socket where
  readLine s = loop ""
   where loop acc =
           do bstr <- recvBytes s 1
              if | BS.null bstr       -> return (Left ErrorClosed)
                 | BS.head bstr == 10 -> return (Right (acc ++ "\n"))
                 | otherwise          -> loop (acc ++ BSC.unpack bstr)

  readBlock s y = loop (fromIntegral y) BS.empty
    where loop 0 acc = return (Right (BSC.unpack acc))
          loop x acc =
            do bstr <- recvBytes s x
               if | BS.length bstr == x -> loop 0 (acc `BS.append` bstr)
                  | BS.length bstr == 0 -> return (Left ErrorClosed)
                  | otherwise           -> loop (x - BS.length bstr)
                                                (acc `BS.append` bstr)

  writeBlock s str = loop (BSC.pack str)
    where loop x | BS.null x = return (Right ())
                 | otherwise =
                    do amt <- sendBytes s x
                       loop (BS.drop amt x)

  close s = Hans.close s

  closeOnEnd _ _ = return ()

data ServerState = ServerState {
    startTime     :: String
  , responseCount :: MVar Word64
  , lastHosts     :: MVar [(IP4, Maybe String)]
  }

main :: IO ()
main =
  do startTime     <- formatTime defaultTimeLocale "%c" `fmap` getZonedTime
     responseCount <- newMVar 0
     lastHosts     <- newMVar []
     startServer ServerState { .. }

initEthernetDevice :: NetworkStack -> IO Mac
initEthernetDevice ns =
  do let mac = Mac 0x52 0x54 0x00 0x12 0x34 0x89
     Just dev <- openTapDevice "tap6"
     addDevice ns mac (tapSend dev) (tapReceiveLoop dev)
     return mac

startServer :: ServerState -> IO ()
startServer state =
  do ns  <- newNetworkStack
     mac <- initEthernetDevice ns
     deviceUp ns mac
     putStrLn ("Starting server on device " ++ show mac)

     mbIP <- dhcpDiscover ns mac
     case mbIP of
       Nothing -> putStrLn "Couldn't get an IP address."
       Just ipaddr -> do
         putStrLn ("Device " ++ show mac ++ " has IP " ++ show ipaddr)

         lsock <- listen ns undefined 80 `catch` handler
         forever $ do
           sock <- accept lsock
           putStrLn "Accepted socket."
           _ <- forkIO (handleClient sock state)
           _ <- forkIO (addHost ns (sockRemoteHost sock) (lastHosts state))
           return ()

 where
  handler ListenError{} =
    do putStrLn ("Unable to listen on port 80\n")
       threadDelay (5 * 1000000)
       exitFailure

handleClient :: Socket -> ServerState -> IO ()
handleClient sock state =
  do mreq <- receiveHTTP sock
     case mreq of
       Left err  -> putStrLn ("ReqERROR: " ++ show err)
       Right req ->
         do bod <- buildBody req state
            putStrLn "Built response"
            let lenstr = show (length bod)
                keepAlive = [ mkHeader HdrConnection "keep-alive"
                            | hdr <- retrieveHeaders HdrConnection req
                            , map toLower (hdrValue hdr) == "keep-alive" ]
                conn | null keepAlive = [ mkHeader HdrConnection "Close" ]
                     | otherwise      = keepAlive
                resp = Response {
                             rspCode = (2,0,0)
                           , rspReason = "OK"
                           , rspHeaders = mkHeader HdrContentLength lenstr
                                        : mkHeader HdrContentType   "text/html"
                                        : conn
                           , rspBody = bod
                           }
            respondHTTP sock resp
            if null keepAlive
               then Hans.close sock
               else handleClient sock state

buildBody :: Request String -> ServerState -> IO String
buildBody _req state =
  do numReqs <- modifyMVar (responseCount state) (\ x -> return (x + 1, x))
     prevHosts <- readMVar (lastHosts state)
     putStrLn ("prevHosts: " ++ show prevHosts ++ "\n")
     return $ renderHtml $
       docTypeHtml $ do
         H.head (title "HaLVM Test Server")
         body $ do
           h1 "Hi!"
           p $ do "I'm a HaLVM. Technically I'm " >> string compilerName
                  " version " >> string (showVersion compilerVersion)
                  ", ported to run on Xen. I started running on "
                  string (startTime state) >> ". You didn't know there was a "
                  "HaLVM standard time, did you? Well, there is. Really."
           p $ do "I have responded to " >> (string (show numReqs)) >> " "
                  "requests since I started, not including yours."
           p $ do "I am using: "
           ul $ do
             li $    hans >> " to talk to you over TCP."
             li $ do http >> " (modified to use the " >> network_hans
                     " shim) to understand your request and respond to it."
             li $ do blaze >> " (which built out of the box) to generate this "
                     "pretty HTML."
             li $ do "... and a host of other libraries to do other, more "
                     "standard, things."
           p $ do "Here are the last 5 hosts that I did something for:"
           ol $ forM_ prevHosts $ \ x -> li (renderHost x)
 where
  hans, http, blaze, network_hans :: Html
  hans  = a ! href "http://hackage.haskell.org/package/hans" $ "HaNS"
  http  = a ! href "http://hackage.haskell.org/package/HTTP" $ "HTTP"
  blaze = a ! href "http://hackage.haskell.org/package/blaze-html"$"blaze-html"
  network_hans = a ! href "http://github.com/GaloisInc/network-hans" $
                   "network-hans"

renderHost :: (IP4, Maybe String) -> Html
renderHost (addr, Nothing) = string (show addr)
renderHost (addr, Just n) = string (n ++ " (" ++ show addr ++ ")")

addHost :: NetworkStack -> IP4 -> MVar [(IP4, Maybe String)] -> IO ()
addHost ns addr hostsMV =
  do putStrLn ("addHost " ++ show addr)
     entry <- catch (name `fmap` getHostByAddr ns addr) handleDnsError
     list  <- takeMVar hostsMV
     putStrLn ("list: " ++ show list)
     case lookup addr list of
       Just _  -> putMVar hostsMV list >> putStrLn "put old list"
       Nothing -> putMVar hostsMV (take 5 ((addr, entry) : list )) >> putStrLn "put new list"
 where
  name = Just . hostName
  handleDnsError :: DnsException -> IO (Maybe String)
  handleDnsError e = print e >> return Nothing
