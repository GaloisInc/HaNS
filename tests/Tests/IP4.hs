module Tests.IP4 (ip4Tests) where

import Tests.IP4.Fragmentation (fragTests)
import Tests.IP4.Icmp4 (icmp4Tests)
import Tests.IP4.Packet (packetTests)

import Test.Tasty


ip4Tests :: TestTree
ip4Tests  = testGroup "IP4"
  [ packetTests
  , fragTests
  , icmp4Tests
  ]
