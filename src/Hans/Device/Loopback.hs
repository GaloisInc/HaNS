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
  do let devConfig = defaultDeviceConfig { dcSendQueueLen    = 1
                                         , dcChecksumOffload = True }
     let devName   = "lo"
     let devMac    = Mac 0 0 0 0 0 0
     devStats     <- newDeviceStats
     devSendQueue <- newBoundedChan (dcSendQueueLen devConfig)

     return $! Device { devStart   = return ()
                      , devStop    = return ()
                      , devCleanup = return ()
                      , .. }
