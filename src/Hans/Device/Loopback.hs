{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hans.Device.Loopback where

import Hans.Device.Types
import Hans.Ethernet.Types (Mac(..))
import Hans.Types


import Control.Concurrent.BoundedChan (newBoundedChan)



-- | A device that just posts outgoing packets back to the input queue.
newLoopbackDevice :: NetworkStack -> IO Device
newLoopbackDevice _ns =
  do let noChecksum = ChecksumOffload { coIP4   = True
                                      , coUdp   = True
                                      , coTcp   = True
                                      , coIcmp4 = True }

     let devConfig = defaultDeviceConfig { dcSendQueueLen = 1
                                         , dcTxOffload    = noChecksum
                                         , dcRxOffload    = noChecksum }
     let devName   = "lo"
     let devMac    = Mac 0 0 0 0 0 0

     devStats     <- newDeviceStats
     devSendQueue <- newBoundedChan (dcSendQueueLen devConfig)

     return $! Device { devStart   = return ()
                      , devStop    = return ()
                      , devCleanup = return ()
                      , .. }
