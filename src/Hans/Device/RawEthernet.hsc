{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hans.Device.RawEthernet (listDevices,openDevice) where

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

import           Hans.Ethernet.Types (Mac(..))
import           Hans.Device.Types
import           Hans.Threads (forkNamed)
import           Hans.Types (NetworkStack(..),InputPacket(..))

import           Control.Concurrent
                     (threadWaitRead,killThread,newMVar,modifyMVar_)
import           Control.Concurrent.BoundedChan
                     (BoundedChan,newBoundedChan,readChan,tryWriteChan)
import qualified Control.Exception as X
import           Control.Monad (forever,when,foldM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import           Data.Word (Word8)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CSize(..),CLong(..),CInt(..),CChar(..))
import           Foreign.Marshal.Alloc (allocaBytes, alloca)
import           Foreign.Marshal.Array (allocaArray,peekArray)
import           Foreign.Ptr (Ptr,plusPtr)
import           Foreign.Storable (pokeByteOff, peek)
import           System.Posix.Types (Fd(..))

listDevices :: IO [DeviceName]
listDevices  = pure []

openDevice :: NetworkStack -> DeviceName -> DeviceConfig -> IO Device
openDevice ns devName devConfig =
  do (fd, devMac, ifIdx) <- initEthernetDevice devName

     -- The `starting` lock makes sure that only one set of threads will be
     -- started at once, while the `running` var holds the ids of the running
     -- threads.
     threadIds <- newMVar Nothing

     devStats <- newDeviceStats

     devSendQueue <- newBoundedChan (dcSendQueueLen devConfig)

     let dev = Device { .. }

         devStart = modifyMVar_ threadIds $ \ mbTids ->
           case mbTids of

             Nothing ->
               do recvThread <- forkNamed "ethernetRecvLoop"
                      (ethernetRecvLoop ns dev fd)

                  sendThread <- forkNamed "ethernetSendLoop"
                      (ethernetSendLoop devStats fd devSendQueue ifIdx)

                  return $ Just (recvThread,sendThread)

             Just {} ->
                  return mbTids

         devStop = modifyMVar_ threadIds $ \ mbTids ->
           case mbTids of
             Just (recvThread,sendThread) ->
               do killThread recvThread
                  killThread sendThread
                  return Nothing

             Nothing ->
                  return Nothing

         devCleanup =
           do ethernetClose fd

     return dev

initEthernetDevice :: DeviceName -> IO (Fd,Mac,CInt)
initEthernetDevice devName =
  do (fd,[a,b,c,d,e,f], cint) <-
        allocaArray 6 $ \ macPtr ->
          alloca $ \ cintPtr ->
            do fd <- S.unsafeUseAsCString devName $ \devNamePtr ->
                       c_init_ethernet_device devNamePtr macPtr cintPtr
               mac <- peekArray 6 macPtr
               cint <- peek cintPtr
               return (fd,mac,cint)

     when (fd < 0) $ X.throwIO (FailedToOpen devName)

     return (fd, Mac a b c d e f, cint)


ethernetSendLoop :: DeviceStats -> Fd -> BoundedChan L.ByteString -> CInt -> IO ()
ethernetSendLoop stats fd queue ifIdx = forever $ do
     -- Drop both the SRC and DEST Mac address
  bs <- L.toStrict <$> readChan queue
  let destMac = S.take 6 bs
      payload = bs
  bytesWritten <- S.unsafeUseAsCStringLen payload $ \(ptr, clen) ->
     S.unsafeUseAsCStringLen destMac $ \(destMac, _) ->
       sendtosocket fd ptr (fromIntegral clen) ifIdx destMac
  if fromIntegral bytesWritten == S.length bs
    then do
      updateBytes statTX stats (fromIntegral bytesWritten)
      updatePackets statTX stats
    else
      updateError statTX stats

ethernetRecvLoop :: NetworkStack -> Device -> Fd -> IO ()
ethernetRecvLoop ns dev @ Device { .. } fd = forever $
  do threadWaitRead fd

     bytes <- S.createUptoN 1514 $ \ ptr ->
       do actual <- c_read fd ptr 1514
          return (fromIntegral actual)

     success <- tryWriteChan (nsInput ns) $! FromDevice dev bytes
     if success
        then do updateBytes   statRX devStats (S.length bytes)
                updatePackets statRX devStats

        else updateError statRX devStats


ethernetClose :: Fd -> IO ()
ethernetClose = c_close

foreign import ccall unsafe "init_ethernet_device"
  c_init_ethernet_device :: CString -> Ptr Word8 -> Ptr CInt -> IO Fd

foreign import ccall safe "send_to_socket"
  sendtosocket
    :: Fd    -- ^ Socket file descriptor
    -> Ptr a -- ^ Buffer
    -> CSize -- ^ Buffer size
    -> CInt  -- ^ Interface index
    -> Ptr CChar
    -> IO CInt -- ^ Returns bytes written

foreign import ccall safe "read"
  c_read :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall safe "close"
  c_close :: Fd -> IO ()
