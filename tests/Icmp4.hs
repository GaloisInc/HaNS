module Icmp4 where

import Icmp4.Packet (icmp4PacketTests)

import Test.Framework (Test,testGroup)


icmp4Tests :: Test
icmp4Tests  = testGroup "icmp4"
  [ icmp4PacketTests
  ]
