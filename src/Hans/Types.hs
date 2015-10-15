module Hans.Types where

import Hans.Queue
import Hans.Device

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.STM (TVar)
import qualified Data.ByteString as S


data NetworkStackConfig =
  NetworkStackConfig { nscInputQueueSize :: {-# UNPACK #-} !Int
                       -- ^ The size of the device input queue
                     }

defaultNetworkStackConfig :: NetworkStackConfig
defaultNetworkStackConfig  =
  NetworkStackConfig { nscInputQueueSize = 128
                     }

data NetworkStack = NetworkStack { nsInput :: !(Queue S.ByteString)
                                   -- ^ The input packet queue

                                 , nsDevices :: {-# UNPACK #-} !(TVar [Device])
                                   -- ^ All registered devices

                                 , nsPacketThread :: !ThreadId
                                   -- ^ The id of the thread that is processing
                                   -- incoming packets
                                 }
