{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hans.Device.Tap (listDevices,openDevice) where

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

import           Hans.Ethernet.Types (Mac(..))
import           Hans.Device.Types

import           Control.Concurrent
                     (threadWaitRead,forkIO,killThread,newMVar,withMVar)
import           Control.Concurrent.BoundedChan
                     (BoundedChan,newBoundedChan,readChan,tryWriteChan)
import qualified Control.Exception as X
import           Control.Monad (forever,when,foldM_)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import           Data.IORef (newIORef,atomicModifyIORef',readIORef,writeIORef)
import           Data.Word (Word8)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CSize(..),CLong(..),CInt(..),CChar(..))
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Marshal.Array (allocaArray,peekArray)
import           Foreign.Ptr (Ptr,plusPtr)
import           Foreign.Storable (pokeByteOff)
import           System.Posix.Types (Fd(..))


-- | Not sure how this should work yet... Should it only ever show tap device
-- names? Maybe this should return a singleton list of an ephemeral device?
listDevices :: IO [DeviceName]
listDevices  = return []

openDevice :: DeviceName -> DeviceConfig -> BoundedChan InputPacket -> IO Device
openDevice devName devConfig devRecvQueue =
  do (fd,devMac) <- initTapDevice devName

     -- The `starting` lock makes sure that only one set of threads will be
     -- started at once, while the `running` var holds the ids of the running
     -- threads.
     lock      <- newMVar ()
     threadIds <- newIORef Nothing

     devStats <- newDeviceStats

     devSendQueue <- newBoundedChan (dcSendQueueLen devConfig)

     let dev = Device { .. }

         devStart = withMVar lock $ \ () ->
           do mbTids <- readIORef threadIds

              when (mbTids == Nothing) $
                do recvThread <- forkIO (tapRecvLoop dev      fd devRecvQueue)
                   sendThread <- forkIO (tapSendLoop devStats fd devSendQueue)
                   writeIORef threadIds (Just (recvThread,sendThread))

         devStop = withMVar lock $ \ () ->
           do mb <- atomicModifyIORef' threadIds ( \mb -> (Nothing, mb) )
              case mb of
                Just (recvThread,sendThread) ->
                  do killThread recvThread
                     killThread sendThread

                Nothing ->
                     return ()

         devCleanup =
           do tapClose fd

     return dev

initTapDevice :: DeviceName -> IO (Fd,Mac)
initTapDevice devName =
  do (fd,[a,b,c,d,e,f]) <-
         allocaArray 6 $ \ macPtr ->
             do fd <- S.unsafeUseAsCString devName $ \ devNamePtr ->
                          c_init_tap_device devNamePtr macPtr

                mac <- peekArray 6 macPtr
                return (fd,mac)

     when (fd < 0) (X.throwIO (FailedToOpen devName))

     return (fd, Mac a b c d e f)


-- | Send a packet out over the tap device.
tapSendLoop :: DeviceStats -> Fd -> BoundedChan L.ByteString -> IO ()
tapSendLoop stats fd queue = forever $
  do bs <- readChan queue

     let chunks = L.toChunks bs
         len    = length chunks

     allocaBytes (fromIntegral ((#size struct iovec) * len)) $ \ iov ->
       do foldM_ writeChunk iov chunks
          bytesWritten <- c_writev fd iov (fromIntegral len)
          updateTX stats (fromIntegral bytesWritten == L.length bs)
  where

  -- write the chunk address and length into the iovec entry
  writeChunk iov chunk =
    do S.unsafeUseAsCStringLen chunk $ \ (ptr,clen) ->
              writeIOVec iov ptr (fromIntegral clen)

       return (iov `plusPtr` (#size struct iovec))


-- | Receive a packet from the tap device.
tapRecvLoop :: Device -> Fd -> BoundedChan InputPacket -> IO ()
tapRecvLoop dev @ Device { .. } fd queue = forever $
  do threadWaitRead fd

     ipBytes <- S.createAndTrim 1514 $ \ ptr ->
       do actual <- c_read fd ptr 1514
          return (fromIntegral actual)

     -- tap devices don't appear to pad received packets out to the minimum size
     -- of 60 bytes, so just don't do that check here.

     let input = InputPacket { ipDevice = dev, .. }
     success <- tryWriteChan queue $! input
     updateRX devStats success


tapClose :: Fd -> IO ()
tapClose fd =
  do c_close fd


-- Foreign Interface -----------------------------------------------------------

foreign import ccall unsafe "init_tap_device"
  c_init_tap_device :: CString -> Ptr Word8 -> IO Fd

type IOVec = ()

writeIOVec :: Ptr IOVec -> Ptr CChar -> CSize -> IO ()
writeIOVec iov ptr len =
  do (#poke struct iovec, iov_base) iov ptr
     (#poke struct iovec, iov_len)  iov len


foreign import ccall unsafe "writev"
  c_writev :: Fd -> Ptr IOVec -> CSize -> IO CLong

foreign import ccall safe "read"
  c_read :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall safe "close"
  c_close :: Fd -> IO ()
