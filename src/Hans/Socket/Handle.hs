{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hans.Socket.Handle(makeHansHandle) where

import           Control.Concurrent(threadDelay)
import           Control.Exception(throwIO)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import           Data.Typeable(Typeable)
import           Foreign.Ptr(Ptr, castPtr, plusPtr)
import           GHC.IO.Buffer(newByteBuffer)
import           GHC.IO.BufferedIO(BufferedIO(..),
                                   readBuf, readBufNonBlocking,
                                   writeBuf, writeBufNonBlocking)
import           GHC.IO.Device(IODevice(..), RawIO(..), IODeviceType(..))
import           GHC.IO.Handle(mkFileHandle, noNewlineTranslation)
import           Hans.Network(Network(..))
import           Hans.Socket(Socket(..), DataSocket(..))
import           Prelude hiding (read)
import           System.IO(Handle, IOMode)


instance (Socket sock, DataSocket sock, Network addr) =>
            IODevice (sock addr) where
  ready dev forWrite msecs =
    do let tester = if forWrite then sCanWrite else sCanRead
       canDo <- tester dev
       if | canDo      -> return True
          | msecs <= 0 -> return False
          | otherwise  -> do let delay = min msecs 100
                             threadDelay (delay * 1000)
                             ready dev forWrite (msecs - delay)
  close bs     = sClose bs
  isTerminal _ = return False
  isSeekable _ = return False
  seek _ _ _   = throwIO (userError "Seek on HaNS socket.")
  tell _       = throwIO (userError "Tell on HaNS socket.")
  getSize _    = throwIO (userError "getSize on HaNS socket.")
  setSize _ _  = throwIO (userError "setSize on HaNS socket.")
  setEcho _ _  = throwIO (userError "setEcho on HaNS socket.")
  getEcho _    = throwIO (userError "getEcho on HaNS socket.")
  setRaw _ _   = return ()
  devType _    = return Stream
  dup _        = throwIO (userError "dup on HaNS socket.")
  dup2 _ _     = throwIO (userError "dup2 on HaNS socket.")

instance (Socket sock, DataSocket sock, Network addr) =>
            RawIO (sock addr) where
  read sock dptr sz =
    do bstr <- sRead sock (fromIntegral sz)
       copyToPtr dptr sz bstr
  readNonBlocking sock dptr sz =
    do mbstr <- sTryRead sock (fromIntegral sz)
       case mbstr of
         Nothing   -> return Nothing
         Just bstr -> Just `fmap` copyToPtr dptr sz bstr
  write sock ptr sz =
    do bstr <- BSS.packCStringLen (castPtr ptr, sz)
       sendAll (BS.fromStrict bstr)
   where
    sendAll bstr
      | BS.null bstr = return ()
      | otherwise    = do num <- sWrite sock bstr
                          sendAll (BS.drop (fromIntegral num) bstr)
  writeNonBlocking sock ptr sz =
    do bstr <- BSS.packCStringLen (castPtr ptr, sz)
       num  <- sWrite sock (BS.fromStrict bstr)
       return (fromIntegral num)

instance (Socket sock, DataSocket sock, Network addr) =>
            BufferedIO (sock addr) where
  newBuffer         _ = newByteBuffer (64 * 1024)
  fillReadBuffer      = readBuf
  fillReadBuffer0     = readBufNonBlocking
  flushWriteBuffer    = writeBuf
  flushWriteBuffer0   = writeBufNonBlocking

-- |Make a GHC Handle from a Hans handle.
makeHansHandle :: (Socket sock, DataSocket sock, Network addr, Typeable sock) =>
                  sock addr -> IOMode -> IO Handle
makeHansHandle socket mode =
  mkFileHandle socket "<socket>" mode Nothing noNewlineTranslation

copyToPtr :: Num a => Ptr b -> Int -> BS.ByteString -> IO a
copyToPtr ptr sz bstr
  | BS.length bstr == 0              = return 0
  | BS.length bstr > fromIntegral sz = fail "Too big a chunk for copy!"
  | otherwise                        =
      do copyBS (BS.toChunks bstr) ptr sz
         return (fromIntegral (BS.length bstr))

copyBS :: [BSS.ByteString] -> Ptr a -> Int -> IO ()
copyBS [] _ _ = return ()
copyBS (f:rest) sptr szLeft
  | BSS.null f   = copyBS rest sptr szLeft
  | szLeft <= 0  = return ()
  | otherwise    =
      do let (chunk1, chunk2) = BSS.splitAt szLeft f
             amt              = fromIntegral (BSS.length chunk1)
         BSS.useAsCString chunk1 $ \ dptr -> memcpy dptr sptr amt
         copyBS (chunk2 : rest) (sptr `plusPtr` amt) (szLeft - amt)

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> Int -> IO ()
