{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Hans.Device (
    module Exports,
    closeDevice
  ) where

#if   defined(HANS_TARGET_UNIX)
import           Hans.Device.Tap as Exports (listDevices,openDevice)
#elif defined(HANS_TARGET_XEN)
import           Hans.Device.Xen as Exports (listDevices,openDevice)
#endif

import           Hans.Device.Types as Exports


closeDevice :: Device -> IO ()
closeDevice Device { .. } =
  do devStop
     devCleanup
