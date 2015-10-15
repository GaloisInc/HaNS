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

data Device = Device { devName :: !DeviceName
                       -- ^ Readable name for this device

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
