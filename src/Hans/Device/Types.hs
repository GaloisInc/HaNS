{-# LANGUAGE DeriveDataTypeable #-}

module Hans.Device.Types where

import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

data Device = Device { devName     :: !DeviceName
                     , devSend     :: L.ByteString -> IO ()
                     , devRecvLoop :: (S.ByteString -> IO ()) -> IO ()
                     , devClose    :: IO ()
                     , devChecksum :: Bool
                     }

data DeviceException = FailedToOpen !DeviceName
                       deriving (Typeable,Show)

instance X.Exception DeviceException
