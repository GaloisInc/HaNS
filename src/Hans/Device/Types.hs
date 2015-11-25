{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Types where

import           Hans.Ethernet.Types (Mac)

import           Control.Concurrent.BoundedChan (BoundedChan)
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

-- | Static configuration data for creating a device.
data DeviceConfig = DeviceConfig { dcSendQueueLen :: {-# UNPACK #-} !Int
                                   -- ^ How large the send queue should be.

                                 , dcChecksumOffload :: !Bool
                                   -- ^ Whether or not the checksum calculation
                                   -- has been offloaded to the device.

                                 , dcMtu :: !Int
                                 }

defaultDeviceConfig :: DeviceConfig
defaultDeviceConfig  = DeviceConfig { dcSendQueueLen    = 128
                                    , dcChecksumOffload = False
                                    , dcMtu             = 1500
                                    }

data Device = Device { devName :: !DeviceName
                       -- ^ The name of this device

                     , devMac :: !Mac
                       -- ^ The mac address associated with this device

                     , devConfig :: !DeviceConfig
                       -- ^ Static configuration information for this device

                     , devSendQueue :: !(BoundedChan L.ByteString)
                       -- ^ Outgoing message queue for this device

                     , devStart :: !(IO ())
                       -- ^ Start packet flow

                     , devStop :: !(IO ())
                       -- ^ Stop packet flow

                     , devCleanup :: !(IO ())
                       -- ^ Cleanup resources associated with a 'Device'

                     , devStats :: !DeviceStats
                       -- ^ Statistics about this device
                     }

-- Devices are compared by mac address
instance Eq Device where
  a == b = devMac a == devMac b
  a /= b = devMac a == devMac b

  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance Ord Device where
  compare a b = compare (devMac a) (devMac b)
  {-# INLINE compare #-}


data DeviceException = FailedToOpen !DeviceName
                       deriving (Typeable,Show)

instance X.Exception DeviceException


-- Statistics ------------------------------------------------------------------

type Stat = IORef Int

incrementStat :: Stat -> IO ()
incrementStat ref = atomicModifyIORef' ref (\ i -> (i + 1, ()))

data DeviceStats = DeviceStats { statTX      :: !Stat
                               , statRX      :: !Stat
                               , statDropped :: !Stat
                               , statError   :: !Stat
                               }

newDeviceStats :: IO DeviceStats
newDeviceStats  =
  do statTX      <- newIORef 0
     statRX      <- newIORef 0
     statDropped <- newIORef 0
     statError   <- newIORef 0
     return DeviceStats { .. }

dumpStats :: DeviceStats -> IO ()
dumpStats DeviceStats { .. } =
  do tx <- readIORef statTX
     rx <- readIORef statRX
     d  <- readIORef statDropped
     e  <- readIORef statError
     putStrLn $ unlines
       [ "tx:      " ++ show tx
       , "rx:      " ++ show rx
       , "dropped: " ++ show d
       , "errors:  " ++ show e ]

-- | Add one to the count of dropped packets for this device.
updateDropped :: DeviceStats -> IO ()
updateDropped DeviceStats { .. } = incrementStat statDropped

-- | Add one to the error count for this device.
updateError :: DeviceStats -> IO ()
updateError DeviceStats { .. } = incrementStat statError

-- | Update information about packets received.
updateRX :: DeviceStats -> Bool -> IO ()
updateRX stats success
  | success   = incrementStat (statRX stats)
  | otherwise = updateError stats

-- | Update information about packets sent.
updateTX :: DeviceStats -> Bool -> IO ()
updateTX stats success
  | success   = incrementStat (statTX stats)
  | otherwise = updateError stats
