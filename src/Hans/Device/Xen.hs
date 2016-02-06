module Hans.Device.Xen where

import Hans.Device.Types
import Hans.Threads (forkNamed)

import           Control.Concurrent (newMVar,modifyMVar_,killThread)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Hypervisor.XenStore (XenStore)
import           XenDevice.NIC (NIC,listNICs,openNIC,setReceiveHandler)


listDevices :: XenStore -> IO [DeviceName]
listDevices xs =
  do nics <- listNICs
     return (map S8.pack nics)

openDevice :: XenStore -> NetworkStack -> DeviceName -> DeviceConfig -> IO Device
openDevice xs ns devName devConfig =
  do nic <- openNIC xs (S8.unpack devName)

     devStats     <- newDeviceStats
     devSendQueue <- newBoundedChan (dcSendQueueLen devConfig)

     thread <- newMVar Nothing

     let dev = Device { .. }

         devStart = modifyMVar_ thread $ \ mbTid ->
           case mbTid of

             Nothing ->
               do sendThread <- forkNamed "xenSendLoop"
                      (xenSendLoop ns dev nic)

                  setReceiveHandler nic (xenRecv ns dev)

                  return (Just sendThread)

             Just {} ->
                  return mbTid

         devStop = modifyMVar_ thread $ \ mbTid ->
           case mbTid of

             Just tid ->
               do killThread tid
                  return Nothing

             Nothing ->
                  return Nothing

         -- NOTE: there's no way to cleanup a NIC
         devCleanup =
             return ()

     return dev

     return $!
       Device { devStart   = setReceiveHandler nic (xenRecv ns devSendQueue)
              , devStop    = setReceiveHandler nic (\ _ -> return ())
              , devCleanup = return ()
              }


-- NOTE: No way to update stats here, as we can't tell if sendPacket failed.
xenSendLoop :: NetworkStack -> NIC -> BoundedChan L.ByteString -> IO ()
xenSendLoop ns nic chan = forever $
  do bs <- readChan queue
     sendPacket nic bs

xenRecv :: NetworkStack -> Device -> S.ByteString -> IO ()
xenRecv ns dev bytes =
  do success <- tryWriteChan (nsInput ns) $! FromDevice dev bytes
     if success
        then do updateBytes   statRX devStats (S.length bytes)
                updatePackets statRX devStats

        else updateError statRX devStats
