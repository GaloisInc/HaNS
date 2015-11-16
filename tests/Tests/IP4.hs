module Tests.IP4 (ip4Tests) where

import Tests.IP4.Fragmentation
import Tests.IP4.Packet

import Test.Tasty


ip4Tests :: TestTree
ip4Tests  = testGroup "IP4"
  [ packetTests
  , fragTests
  ]
