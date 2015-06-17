{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hans.Device.Tap (openDevice) where

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

import           Hans.Device.Types

import           Control.Concurrent (threadWaitRead)
import qualified Control.Exception as X
import           Control.Monad (forever,unless,when,foldM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import           Data.Word (Word8)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CSize(..),CLong(..),CInt(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr (Ptr,plusPtr)
import           Foreign.Storable (pokeByteOff)
import           System.Posix.Types (Fd(..))


openDevice :: DeviceName -> IO Device
openDevice devName =
  do fd <- S.unsafeUseAsCString devName c_init_tap_device
     when (fd < 0) (X.throwIO (FailedToOpen devName))

     return Device { devRecvLoop = tapRecvLoop fd
                   , devSend     = tapSend     fd
                   , devClose    = tapClose    fd
                   , devChecksum = True
                   , .. }


-- | Send a packet out over the tap device.
--
-- XXX It would be nice if there was a way to send the chunked data to the
-- kernel, instead of re-allocating a strict packet first.
tapSend :: Fd -> L.ByteString -> IO ()
tapSend fd bs =
  allocaBytes (fromIntegral ((#size struct iovec) * len)) $ \ iov ->
    do foldM_ writeChunk iov chunks
       _bytesWritten <- c_writev fd iov (fromIntegral len)
       return ()
  where
  chunks = L.toChunks bs
  len    = length chunks

  -- write the chunk address and length into the iovec entry
  writeChunk iov chunk =
    do let (fptr, 0, clen) = S.toForeignPtr chunk
       _ <- withForeignPtr fptr $ \ptr ->
              writeIOVec iov ptr (fromIntegral clen)

       return (iov `plusPtr` (#size struct iovec))


tapRecvLoop :: Fd -> (S.ByteString -> IO ()) -> IO ()
tapRecvLoop fd k = forever $
  do threadWaitRead fd

     bytes <- S.createAndTrim 1514 $ \ ptr ->
       do actual <- c_read fd ptr 1514
          return (fromIntegral actual)

     unless (S.length bytes < 60) (k bytes)


tapClose :: Fd -> IO ()
tapClose fd =
  do c_close fd


-- Foreign Interface -----------------------------------------------------------

foreign import ccall unsafe "init_tap_device"
  c_init_tap_device :: CString -> IO Fd

type IOVec = ()

writeIOVec :: Ptr IOVec -> Ptr Word8 -> CSize -> IO ()
writeIOVec iov ptr len =
  do (#poke struct iovec, iov_base) iov ptr
     (#poke struct iovec, iov_len)  iov len


foreign import ccall unsafe "writev"
  c_writev :: Fd -> Ptr IOVec -> CSize -> IO CLong

foreign import ccall safe "read"
  c_read :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall safe "close"
  c_close :: Fd -> IO ()
