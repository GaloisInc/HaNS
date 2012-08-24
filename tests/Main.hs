module Main where

import Tcp (tcpTests)
import Udp (udpTests)

import Test.Framework (defaultMain)


main = defaultMain
  [ tcpTests
  , udpTests
  ]
