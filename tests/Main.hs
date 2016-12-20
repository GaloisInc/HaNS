module Main where

import Tests.Checksum
import Tests.Ethernet
import Tests.IP4
import Tests.Address

import Test.Tasty
import Test.Tasty.Runners (consoleTestReporter)
import Test.Tasty.Runners.AntXML (antXMLRunner)


main :: IO ()
main  = defaultMainWithIngredients [antXMLRunner,consoleTestReporter] $
  testGroup "Properties"
    [ checksumTests
    , addressTests
    , ethernetTests
    , ip4Tests
    ]
