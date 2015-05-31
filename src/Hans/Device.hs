{-# LANGUAGE CPP #-}

module Hans.Device ( Device, openDevice ) where

#if   defined(HANS_TARGET_UNIX)
import           Hans.Device.Tap (openDevice)
#elif defined(HANS_TARGET_XEN)
import           Hans.Device.Xen (openDevice)
#endif

import           Hans.Device.Types (Device(..))


closeDev :: Device -> IO ()
closeDev Device { .. } = devClose
