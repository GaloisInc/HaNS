{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Arp (
    module Exports,
    module Hans.Arp
  ) where

import Hans.Arp.Types as Exports
import Hans.Device (Device(..))
import Hans.Ethernet (EthernetHeader)
import Hans.Monad (Hans,decode)

import qualified Data.ByteString as S


-- | Handle incoming Arp packets.
processArp :: ArpState -> Device -> EthernetHeader -> S.ByteString -> Hans ()
processArp arp dev hdr payload =
  do ArpPacket { .. } <- decode (devStats dev) getArpPacket payload

     case arpOper of

       ArpRequest -> processArpRequest arp

processArpRequest = undefined
