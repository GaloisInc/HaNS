module Hans.UDP.Input where

import Hans.Device (Device)
import Hans.IP4.Packet (IP4)
import Hans.Monad (Hans)
import Hans.UDP.Packet
import Hans.Types

import qualified Data.ByteString as S


processUDP :: NetworkStack -> Device -> IP4 -> IP4 -> S.ByteString -> Hans ()
processUDP ns dev src dst bytes =
  do undefined
