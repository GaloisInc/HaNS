module Main where

import Icmp4 (icmp4Tests)
import IP4 (ip4Tests)
import Tcp (tcpTests)
import Udp (udpTests)

import Test.Framework (defaultMain)


main = defaultMain
  [ tcpTests
  , udpTests
  , icmp4Tests
  , ip4Tests
  ]
