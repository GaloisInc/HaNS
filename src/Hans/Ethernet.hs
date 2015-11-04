{-# LANGUAGE RecordWildCards #-}

module Hans.Ethernet (
    module Exports,
    module Hans.Ethernet
  ) where

import Hans.Device.Types (Device(..),DeviceStats,InputPacket(..))
import Hans.Ethernet.Types as Exports
import Hans.Monad (Hans,decode')

import qualified Data.ByteString as S


-- | Decode an ethernet frame, or fail trying.
decodeEthernet :: InputPacket -> Hans (DeviceStats,EthernetHeader,S.ByteString)
decodeEthernet InputPacket { .. } =
  do let stats = devStats ipDevice
     (hdr,payload) <- decode' stats getEthernetHeader ipBytes
     return (stats,hdr,payload)
