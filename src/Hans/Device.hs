{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Hans.Device (
    module Exports,
    closeDevice,
    startDevice
  ) where

#if   defined(HANS_TARGET_UNIX)
import           Hans.Device.Tap as Exports (listDevices,openDevice)
#elif defined(HANS_TARGET_XEN)
import           Hans.Device.Xen as Exports (listDevices,openDevice)
#elif defined(HANS_TARGET_RAW_ETHERNET)
import           Hans.Device.RawEthernet as Exports (listDevices,openDevice)
#endif

import           Hans.Device.Types as Exports


-- | Stop packets flowing, and cleanup any resources associated with this
-- device.
closeDevice :: Device -> IO ()
closeDevice Device { .. } =
  do devStop
     devCleanup

-- | Start processing packets through this device.
startDevice :: Device -> IO ()
startDevice Device { .. } = devStart
