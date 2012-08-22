module Tcp (
    tcpTests
  ) where

import Tcp.Window

import Test.Framework (Test,testGroup)

tcpTests :: Test
tcpTests  = testGroup "tcp"
  [ tcpWindowTests
  ]
