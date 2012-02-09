module WebServer where

import Control.Concurrent (forkIO,threadDelay)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..),addUTCTime)
import Data.Time.Clock.POSIX (POSIXTime,getPOSIXTime,posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import Hans.Message.Tcp (TcpPort)
import Hans.NetworkStack
    (NetworkStack,tcpHandle,Socket,readLine,sendSocket,acceptSocket,listenPort
    ,closeSocket,SocketError(..))
import System.Exit (exitFailure)
import System.Locale (defaultTimeLocale)
import qualified Control.Exception as X
import qualified Data.ByteString as S

webserver :: NetworkStack -> TcpPort -> IO ()
webserver ns port = body `X.catch` \se -> print (se :: X.SomeException)
  where
  body = do
    start <- getPOSIXTime
    sock  <- initServer ns port
    serverLoop start sock

accept :: Socket -> IO Socket
accept sock = loop
  where
  loop = X.catch (acceptSocket sock) $ \se -> do
    case se of
      AcceptError err -> putStrLn ("Accept error: " ++ err)
      _               -> putStrLn ("Socket error: " ++ show se)
    loop

serverLoop :: POSIXTime -> Socket -> IO ()
serverLoop start sock = loop
  where
  loop = do
    client <- accept sock
    _      <- forkIO (handleClient start client)
    loop

initServer :: NetworkStack -> TcpPort -> IO Socket
initServer ns port = listenPort (tcpHandle ns) port `X.catch` h
  where
  h ListenError{} = do
    putStrLn ("Unable to listen on port: " ++ show port)
    exitFailure
  h se = do
    print se
    exitFailure

handleClient :: POSIXTime -> Socket -> IO ()
handleClient start client = body `X.catch` \se -> print (se :: X.SomeException)
  where
  body = do
    mb <- processRequest client
    case mb of
      Nothing        -> closeSocket client
      Just (url,req) -> do
        sendSocket client =<< makeResponse start url req
        threadDelay 1000000
        closeSocket client

processRequest :: Socket -> IO (Maybe (String,[S.ByteString]))
processRequest sock = do
  ls <- readRequest sock
  case ls of
    []   -> return Nothing
    l:_  -> return (Just (parseUrl l, ls))

crlf :: S.ByteString
crlf  = S.pack [0x0d, 0x0a]

readRequest :: Socket -> IO [S.ByteString]
readRequest sock = loop
  where
  loop = do
    line <- readLine sock
    if line == crlf
       then return []
       else do
         rest <- loop
         return (line:rest)

parseUrl :: S.ByteString -> String
parseUrl  = head . drop 1 . words . toString

fromString :: String -> S.ByteString
fromString  = S.pack . map (toEnum . fromEnum)

toString :: S.ByteString -> String
toString  = map (toEnum . fromEnum) . S.unpack

status200 :: S.ByteString
status200  = fromString "HTTP/1.1 200 OK\r\n"

contentLength :: Int -> S.ByteString
contentLength len = fromString ("Content-Length: " ++ show len)

contentType :: String -> S.ByteString
contentType ty = fromString ("Content-Type: " ++ ty)

response404 :: S.ByteString
response404  = fromString $ concat
  [ "HTTP/1.1 404 Not Found\r\n"
  , "Content-Length: 0\r\n"
  , "\r\n"
  ]

connectionClose :: S.ByteString
connectionClose  = fromString "Connection: close"

makeResponse :: POSIXTime -> String -> [S.ByteString] -> IO S.ByteString
makeResponse start url req
  | url == "/favicon.ico" = return response404
  | otherwise             = do
    uptime <- timePassed start
    let date = posixSecondsToUTCTime start
        body = fromString (concat
               [ "<html><head><title>HaLVM</title></head><body>"
               , "<h1>Welcome to the HaLVM!</h1><br />\r\n\r\n"
               , "Started on: "
               , formatDate date
               , ", and up for "
               , uptime
               , "\r\n<h2>HTTP Request:</h2>\r\n<pre>"
               ]) `S.append` S.concat req
                  `S.append` fromString "</pre></body></html>"
    return $! S.concat
          [ status200
          , contentLength (S.length body), crlf
          , contentType "text/html", crlf
          , connectionClose, crlf
          , crlf
          , body
          ]

formatDate :: UTCTime -> String
formatDate  = formatTime defaultTimeLocale "%c"

zeroUTCTime :: UTCTime
zeroUTCTime  = UTCTime (ModifiedJulianDay 0) 0

timePassed :: POSIXTime -> IO String
timePassed start = do
  now <- getPOSIXTime
  let date@(UTCTime day _) = addUTCTime (now - start) zeroUTCTime
  return $ concat
    [ show (toModifiedJulianDay day)
    , " days, "
    , formatTime defaultTimeLocale "%k hours, %M minutes, %S seconds." date
    ]
