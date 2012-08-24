module Udp where

import Udp.Packet

import Test.Framework (Test,testGroup)


udpTests :: Test
udpTests  = testGroup "udp"
  [ udpPacketTests
  ]
