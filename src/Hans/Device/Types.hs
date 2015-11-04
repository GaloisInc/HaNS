{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Types where

import           Hans.Queue

import           Control.Concurrent.STM (STM,TVar,newTVarIO,modifyTVar')
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

-- | Static configuration data for creating a device.
data DeviceConfig = DeviceConfig { dcSendQueueLen :: {-# UNPACK #-} !Int
                                   -- ^ How large the send queue should be

                                 , dcChecksumOffload :: !Bool
                                   -- ^ Whether or not the checksum calculation
                                   -- has been offloaded to the 
                                 }

defaultDeviceConfig :: DeviceConfig
defaultDeviceConfig  =
  DeviceConfig { dcSendQueueLen    = 128
               , dcChecksumOffload = False
               }

data Device = Device { devName :: !DeviceName
                       -- ^ The name of this device

                     , devConfig :: !DeviceConfig
                       -- ^ Static configuration information for this device

                     , devSendQueue :: !(Queue L.ByteString)
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

data DeviceException = FailedToOpen !DeviceName
                       deriving (Typeable,Show)

instance X.Exception DeviceException


-- Packets ---------------------------------------------------------------------

-- | Packets received from a specific device.
data InputPacket = InputPacket { ipDevice :: !Device
                               , ipBytes  :: !S.ByteString
                               }


-- Statistics ------------------------------------------------------------------

data DeviceStats = DeviceStats { statTX      :: !(TVar Int)
                               , statRX      :: !(TVar Int)
                               , statDropped :: !(TVar Int)
                               , statError   :: !(TVar Int)
                               }

newDeviceStats :: IO DeviceStats
newDeviceStats  =
  do statTX      <- newTVarIO 0
     statRX      <- newTVarIO 0
     statDropped <- newTVarIO 0
     statError   <- newTVarIO 0
     return DeviceStats { .. }

-- | Add one to the count of dropped packets for this device.
updateDropped :: DeviceStats -> STM ()
updateDropped DeviceStats { .. } = modifyTVar' statDropped (+ 1)

-- | Add one to the error count for this device.
updateError :: DeviceStats -> STM ()
updateError DeviceStats { .. } = modifyTVar' statError (+ 1)

-- | Update information about packets received.
updateRX :: DeviceStats -> Bool -> STM ()
updateRX stats success
  | success   = modifyTVar' (statTX stats) (+ 1)
  | otherwise = updateError stats

-- | Update information about packets sent.
updateTX :: DeviceStats -> Bool -> STM ()
updateTX stats success
  | success   = modifyTVar' (statTX stats) (+ 1)
  | otherwise = updateError stats
