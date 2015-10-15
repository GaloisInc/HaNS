{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Types where

import           Hans.Queue

import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

-- | Static configuration data for creating a device.
data DeviceConfig = DeviceConfig { dcSendQueueLen :: {-# UNPACK #-} !Int
                                   -- ^ How large the send queue should be
                                 }

defaultDeviceConfig :: DeviceConfig
defaultDeviceConfig  =
  DeviceConfig { dcSendQueueLen = 128
               }

data Device = Device { devName :: !DeviceName
                       -- ^ The name of this device

                     , devSendQueue :: !(Queue L.ByteString)
                       -- ^ Outgoing message queue for this device

                     , devStart :: !(IO ())
                       -- ^ Start packet flow

                     , devStop :: !(IO ())
                       -- ^ Stop packet flow

                     , devCleanup :: !(IO ())
                       -- ^ Cleanup resources associated with a 'Device'
                     }

data DeviceException = FailedToOpen !DeviceName
                       deriving (Typeable,Show)

instance X.Exception DeviceException


data WithDevice a = WithDevice !Device a
                    deriving (Functor,Foldable,Traversable)
