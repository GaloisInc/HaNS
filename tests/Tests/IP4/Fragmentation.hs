module Tests.IP4.Fragmentation where

import Tests.IP4.Packet
import Tests.Network

import Hans.Config
import Hans.IP4.Fragments
import Hans.IP4.Packet
import Hans.Monad (runHansOnce)

import qualified Data.ByteString.Lazy as L
import           Data.Maybe (catMaybes)
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

     -- XXX there's something funny going on when the MTU is particularly small
     len   <- pick (choose (50,500))
     mtu   <- pick (choose (40,len))
     bytes <- pick (arbitraryPayload len)

     let hdr = emptyIP4Header { ip4DestAddr   = dst
                              , ip4SourceAddr = src
                              , ip4Protocol   = prot
                              , ip4Ident      = ident
                              }

         chunks = [(h,L.toStrict body) | (h,body) <- splitPacket mtu hdr bytes]

     incoming <- pick (shuffle chunks)
     table    <- run (newFragTable defaultConfig)

     results <- run $ sequence [ runHansOnce (processFragment table fhdr body)
                               | (fhdr,body) <- incoming ]

     run (cleanupFragTable table)

     -- we should only get a single successful result here, otherwise something
     -- is wrong with the way that fragments are being collected
     case catMaybes results of
       [(_,result)] -> return (L.fromStrict result == bytes)
       _            -> return False
