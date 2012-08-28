module IP4 where

import IP4.Packet

import Test.Framework (Test,testGroup)


ip4Tests :: Test
ip4Tests  = testGroup "ip4"
  [ ip4PacketTests
  ]
