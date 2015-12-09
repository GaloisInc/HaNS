{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Hans.Device.Types where

import           Hans.Ethernet.Types (Mac)
import           Hans.Lens

import           Control.Concurrent.BoundedChan (BoundedChan)
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Typeable (Typeable)


type DeviceName = S.ByteString

data ChecksumOffload = ChecksumOffload { coIP4   :: !Bool
                                       , coUdp   :: !Bool
                                       , coTcp   :: !Bool
                                       , coIcmp4 :: !Bool
                                       } deriving (Show)

defaultChecksumOffload :: ChecksumOffload
defaultChecksumOffload  = ChecksumOffload { coIP4   = False
                                          , coUdp   = False
                                          , coTcp   = False
                                          , coIcmp4 = False }

-- | Static configuration data for creating a device.
data DeviceConfig = DeviceConfig { dcSendQueueLen :: {-# UNPACK #-} !Int
                                   -- ^ How large the send queue should be.

                                 , dcTxOffload :: !ChecksumOffload
                                 , dcRxOffload :: !ChecksumOffload

                                 , dcMtu :: !Int
                                 } deriving (Show)

class HasDeviceConfig cfg where
  deviceConfig :: Getting r cfg DeviceConfig

instance HasDeviceConfig DeviceConfig where
  deviceConfig = id
  {-# INLINE deviceConfig #-}

instance HasDeviceConfig Device where
  deviceConfig = to devConfig
  {-# INLINE deviceConfig #-}

-- | The TX checksum offload config.
txOffload :: HasDeviceConfig cfg => Getting r cfg ChecksumOffload
txOffload  = deviceConfig . to dcTxOffload

-- | The RX checksum offload config.
rxOffload :: HasDeviceConfig cfg => Getting r cfg ChecksumOffload
rxOffload  = deviceConfig . to dcRxOffload

defaultDeviceConfig :: DeviceConfig
defaultDeviceConfig  = DeviceConfig { dcSendQueueLen = 128
                                    , dcTxOffload    = defaultChecksumOffload
                                    , dcRxOffload    = defaultChecksumOffload
                                    , dcMtu          = 1500
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

addStat :: Stat -> Int -> IO ()
addStat ref n = atomicModifyIORef' ref (\ i -> (i + n, ()))

data StatGroup = StatGroup { _statBytes   :: !Stat
                           , _statPackets :: !Stat
                           , _statErrors  :: !Stat
                           , _statDropped :: !Stat
                           }

statBytes, statPackets, statErrors, statDropped :: Getting r StatGroup Stat
statBytes   = to _statBytes
statPackets = to _statPackets
statErrors  = to _statErrors
statDropped = to _statDropped

newStatGroup :: IO StatGroup
newStatGroup  =
  do _statBytes   <- newIORef 0
     _statPackets <- newIORef 0
     _statErrors  <- newIORef 0
     _statDropped <- newIORef 0
     return $! StatGroup { .. }

dumpStatGroup :: String -> StatGroup -> IO ()
dumpStatGroup pfx = \ StatGroup { .. } ->
  do putStrLn header
     mapM_ showStat [_statBytes,_statPackets,_statErrors,_statDropped]
     putStrLn ""
  where
  header = unwords (map pad [ pfx ++ " bytes", "packets", "errors", "dropped" ])
  pad xs = xs ++ replicate (19 - length xs) ' '

  showStat ref =
    do val <- readIORef ref
       putStr (pad (show val))
       putStr " "
{-# INLINE dumpStatGroup #-}

data DeviceStats = DeviceStats { _statTX :: !StatGroup
                               , _statRX :: !StatGroup
                               }

statTX, statRX :: Getting r DeviceStats StatGroup
statTX = to _statTX
statRX = to _statRX

newDeviceStats :: IO DeviceStats
newDeviceStats  =
  do _statTX <- newStatGroup
     _statRX <- newStatGroup
     return $! DeviceStats { .. }

dumpStats :: DeviceStats -> IO ()
dumpStats DeviceStats { .. } =
  do dumpStatGroup "RX:" _statRX
     dumpStatGroup "TX:" _statTX


-- | Add one to the count of dropped packets for this device.
updateDropped :: Getting Stat DeviceStats StatGroup -> DeviceStats -> IO ()
updateDropped group stats = incrementStat (view (group . statDropped) stats)

-- | Add one to the error count for this device.
updateError :: Getting Stat DeviceStats StatGroup -> DeviceStats -> IO ()
updateError group stats = incrementStat (view (group . statErrors) stats)

-- | Update information about bytes received.
updateBytes :: Getting Stat DeviceStats StatGroup -> DeviceStats -> Int -> IO ()
updateBytes group stats n = addStat (view (group . statBytes) stats) n

-- | Update information about bytes received.
updatePackets :: Getting Stat DeviceStats StatGroup -> DeviceStats -> IO ()
updatePackets group stats = incrementStat (view (group . statPackets) stats)
