module Tests.IP4.Fragmentation where

import Tests.IP4.Packet

import Hans.Config
import Hans.IP4.Fragments
import Hans.IP4.Packet
import Hans.Monad (runHansOnce)

import qualified Data.ByteString.Lazy as L
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


fragTests :: TestTree
fragTests  = testGroup "Fragmentation"
  [ testProperty "Reassembly" propReassemble
  ]


propReassemble :: Property
propReassemble  = monadicIO $
  do src   <- pick arbitraryIP4
     dst   <- pick arbitraryIP4
     prot  <- pick arbitraryProtocol
     ident <- pick arbitraryIdent
     bytes <- pick (arbitraryPayload 1481 4096)

     let hdr = emptyIP4Header { ip4DestAddr   = dst
                              , ip4SourceAddr = src
                              , ip4Protocol   = prot
                              , ip4Ident      = ident
                              }

         chunks = [ (hdr,L.toStrict body)
                  | (hdr,body) <- splitPacket 1500 hdr bytes ]

     incoming <- pick (shuffle chunks)
     table    <- run (newFragTable defaultConfig)

     results <- run $ sequence [ runHansOnce (processFragment table fhdr body)
                               | (fhdr,body) <- incoming ]

     run (cleanupFragTable table)

     case last results of
       Just (_,result) -> return (L.fromStrict result == bytes)
       Nothing         -> return False
