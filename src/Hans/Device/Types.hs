{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Types where

import           Hans.Queue

import           Control.Concurrent.STM (atomically,TVar)
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

data Device = Device { devName      :: !DeviceName
                     , devSendQueue :: !(Queue L.ByteString)
                     , devRecvQueue :: !(Queue S.ByteString)
                     , devStats     :: !(TVar DeviceStats)
                     , devClose     :: !(IO ())
                     , devUp        :: !(IO ())
                     , devDown      :: !(IO ())
                     }

data DeviceStats = DeviceState { packetsDropped :: {-# UNPACK #-} !Int
                               } deriving (Show)

initialStats :: DeviceStats
initialStats  = DeviceStats { packetsDropped = 0 }

-- | Modify the stats held about a device.
updateStats :: (DeviceStats -> DeviceStats) -> Device -> IO ()
updateStats f Device { .. } = atomically (modifyTVar' devStats f)

data DeviceException = FailedToOpen !DeviceName
                       deriving (Typeable,Show)

instance X.Exception DeviceException


data WithDevice a = WithDevice !Device a
                    deriving (Functor,Foldable,Traversable)
