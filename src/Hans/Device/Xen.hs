{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Xen where

import Hans.Device.Types
import Hans.Ethernet.Types (readMac)
import Hans.Threads (forkNamed)
import Hans.Types

import           Control.Concurrent (newMVar,modifyMVar_,killThread)
import           Control.Concurrent.BoundedChan
                     (BoundedChan,newBoundedChan,tryWriteChan,readChan)
import           Control.Monad (forever)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Hypervisor.XenStore (XenStore)
import           XenDevice.NIC (NIC,listNICs,openNIC,setReceiveHandler,sendPacket)


listDevices :: XenStore -> IO [DeviceName]
listDevices xs =
  do nics <- listNICs xs
     return (map S8.pack nics)

openDevice :: XenStore -> NetworkStack -> DeviceName -> DeviceConfig -> IO Device
openDevice xs ns devName devConfig =
  do let macStr = S8.unpack devName
     nic <- openNIC xs macStr

     devStats     <- newDeviceStats
     devSendQueue <- newBoundedChan (dcSendQueueLen devConfig)

     thread <- newMVar Nothing

     devMac <- case readMac macStr of
                 [(x,_)] -> return x
                 _       -> fail ("Unable to parse mac address: " ++ macStr)

     let dev = Device { .. }

         devStart = modifyMVar_ thread $ \ mbTid ->
           case mbTid of

             Nothing ->
               do sendThread <- forkNamed "xenSendLoop"
                      (xenSendLoop nic devSendQueue)

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


-- NOTE: No way to update stats here, as we can't tell if sendPacket failed.
xenSendLoop :: NIC -> BoundedChan L.ByteString -> IO ()
xenSendLoop nic chan = forever $
  do bs <- readChan chan
     sendPacket nic bs

xenRecv :: NetworkStack -> Device -> L.ByteString -> IO ()
xenRecv ns dev @ Device { .. } = \ bytes ->
  do let bytes' = L.toStrict bytes
     success <- tryWriteChan (nsInput ns) $! FromDevice dev bytes'
     if success
        then do updateBytes   statRX devStats (S.length bytes')
                updatePackets statRX devStats

        else updateError statRX devStats
