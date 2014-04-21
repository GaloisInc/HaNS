{-# LANGUAGE ForeignFunctionInterface #-}

module Hans.Device.Tap where

import Hans.Layer.Ethernet
import Hans.Utils (strict,DeviceName)

import Control.Concurrent (threadWaitRead)
import Control.Monad (forever)
import Data.Word (Word8)
import Foreign.C.String (CString,withCString)
import Foreign.C.Types (CLong(..),CSize(..),CInt(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO(hIsReadable)
import System.Posix.Types (Fd(..))
import System.Posix.IO(fdToHandle)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy     as L

import Foreign.C.Error

-- | Open a device by name.
openTapDevice :: DeviceName -> IO (Maybe Fd)
openTapDevice ""   = return Nothing
openTapDevice name = withCString name $ \str -> do
  ret <- c_init_tap_device str
  if ret < 0 then return Nothing else return (Just ret)


-- | Send an ethernet frame via a tap device.
--
-- TODO: make more use of the lazy bytestring
tapSend :: Fd -> L.ByteString -> IO ()
tapSend fd packet = do
  let (fptr, 0, len) = S.toForeignPtr (strict packet)
  _res <- withForeignPtr fptr $ \ptr -> c_write fd ptr (fromIntegral len)
  -- XXX: make sure to continue sending if res < len
  return ()


-- | Fork a reciever loop, and return an IO action to kill the running thread.
tapReceiveLoop :: Fd -> EthernetHandle -> IO ()
tapReceiveLoop fd eh = forever (k =<< tapReceive fd)
  where k pkt = queueEthernet eh pkt

-- | Recieve an ethernet frame from a tap device.
tapReceive :: Fd -> IO S.ByteString
tapReceive fd = do
  threadWaitRead fd
  ready <- hIsReadable =<< fdToHandle fd
  if ready
     then do let packet ptr = fromIntegral `fmap` c_read' fd ptr 1514
             bs <- S.createAndTrim 1514 packet
             if S.length bs <= 14
               then tapReceive fd
               else return bs
     else tapReceive fd

c_read' fd buf size = do
  res <- c_read fd buf size
--  Errno eno <- getErrno
  return res

foreign import ccall unsafe "init_tap_device"
  c_init_tap_device :: CString -> IO Fd

foreign import ccall unsafe "write"
  c_write :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall safe "read"
  c_read :: Fd -> Ptr Word8 -> CSize -> IO CLong

