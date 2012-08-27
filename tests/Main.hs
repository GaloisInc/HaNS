module Main where

import Icmp4 (icmp4Tests)
import Tcp (tcpTests)
import Udp (udpTests)

import Test.Framework (defaultMain)


main = defaultMain
  [ tcpTests
  , udpTests
  , icmp4Tests
  ]
