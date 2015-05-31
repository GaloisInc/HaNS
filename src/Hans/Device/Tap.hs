{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hans.Device.Tap (openDevice) where

import           Hans.Device.Types

import           Control.Concurrent (threadWaitRead)
import qualified Control.Exception as X
import           Control.Monad (forever,unless,when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import           Data.Word (Word8)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CSize(..),CLong(..),CInt(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr)
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
tapSend fd chunks =
  do let (fptr, 0, len) = S.toForeignPtr (L.toStrict chunks)
     _ <- withForeignPtr fptr (\ptr -> c_write fd ptr (fromIntegral len))

     return ()


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

foreign import ccall unsafe "write"
  c_write :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall safe "read"
  c_read :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall safe "close"
  c_close :: Fd -> IO ()
