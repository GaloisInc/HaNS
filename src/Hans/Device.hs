{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Hans.Device (
    Device,
    listDevices,
    openDevice,
    closeDevice
  ) where

#if   defined(HANS_TARGET_UNIX)
import           Hans.Device.Tap (listDevices,openDevice)
#elif defined(HANS_TARGET_XEN)
import           Hans.Device.Xen (listDevices,openDevice)
#endif

import           Hans.Device.Types (Device(..))


closeDevice :: Device -> IO ()
closeDevice Device { .. } = devClose
