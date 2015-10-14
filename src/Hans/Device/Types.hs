{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Types where

import           Hans.Queue

import           Control.Concurrent.STM (atomically,TVar,modifyTVar')
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

data Device = Device { devName      :: !DeviceName
                     , devSendQueue :: !(Queue L.ByteString)
                     , devRecvQueue :: !(Queue S.ByteString)
                     , devClose     :: !(IO ())
                     , devUp        :: !(IO ())
                     , devDown      :: !(IO ())
                     }

data DeviceException = FailedToOpen !DeviceName
                       deriving (Typeable,Show)

instance X.Exception DeviceException


data WithDevice a = WithDevice !Device a
                    deriving (Functor,Foldable,Traversable)
