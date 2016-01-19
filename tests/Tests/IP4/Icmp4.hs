module Tests.IP4.Icmp4 where

import Tests.IP4.Packet (arbitraryIP4)
import Tests.Utils

import Hans.IP4.Icmp4

import qualified Data.ByteString as S
import           Test.Tasty (TestTree,testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
                     (Gen,oneof,arbitraryBoundedRandom,elements,listOf,listOf1
                     ,vectorOf)


-- Packet Generation -----------------------------------------------------------

arbitraryIcmp4Packet :: Gen Icmp4Packet
arbitraryIcmp4Packet  = oneof
  [ do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       c <- arbitraryEchoPayload
       return $! EchoReply a b c

  , do a <- arbitraryDestinationUnreachableCode
       b <- arbitraryPayload
       return $! DestinationUnreachable a b

  , do a <- arbitraryPayload
       return $! SourceQuench a

  , do a <- arbitraryRedirectCode
       b <- arbitraryIP4
       c <- arbitraryPayload
       return $! Redirect a b c

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       c <- arbitraryEchoPayload
       return $! Echo a b c

  , do a <- arbitraryBoundedRandom
       b <- listOf1 arbitraryRouterAddress
       return $! RouterAdvertisement a b

  , return RouterSolicitation

  , do a <- arbitraryTimeExceededCode
       b <- arbitraryPayload
       return $! TimeExceeded a b

  , do a <- arbitraryBoundedRandom
       b <- arbitraryPayload
       return $! ParameterProblem a b

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       c <- arbitraryBoundedRandom
       d <- arbitraryBoundedRandom
       e <- arbitraryBoundedRandom
       return $! Timestamp a b c d e

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       c <- arbitraryBoundedRandom
       d <- arbitraryBoundedRandom
       e <- arbitraryBoundedRandom
       return $! TimestampReply a b c d e

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       return $! Information a b

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       return $! InformationReply a b

  , do a <- arbitraryTraceRouteCode
       b <- arbitraryIdentifier
       c <- arbitraryBoundedRandom
       d <- arbitraryBoundedRandom
       e <- arbitraryBoundedRandom
       f <- arbitraryBoundedRandom
       return $! TraceRoute a b c d e f

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       return $! AddressMask a b

  , do a <- arbitraryIdentifier
       b <- arbitrarySequenceNumber
       c <- arbitraryBoundedRandom
       return $! AddressMaskReply a b c
  ]

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier  = arbitraryBoundedRandom

arbitrarySequenceNumber :: Gen SequenceNumber
arbitrarySequenceNumber  = arbitraryBoundedRandom

arbitraryEchoPayload :: Gen S.ByteString
arbitraryEchoPayload  =
  do bytes <- listOf arbitraryBoundedRandom
     return $! S.pack bytes

arbitraryPayload :: Gen S.ByteString
arbitraryPayload  =
  do bytes <- vectorOf 28 arbitraryBoundedRandom
     return $! S.pack bytes

arbitraryDestinationUnreachableCode :: Gen DestinationUnreachableCode
arbitraryDestinationUnreachableCode  = elements
  [ NetUnreachable
  , HostUnreachable
  , ProtocolUnreachable
  , PortUnreachable
  , FragmentationUnreachable
  , SourceRouteFailed
  , DestinationNetworkUnknown
  , DestinationHostUnknown
  , SourceHostIsolatedError
  , AdministrativelyProhibited
  , HostAdministrativelyProhibited
  , NetworkUnreachableForTOS
  , HostUnreachableForTOS
  , CommunicationAdministrativelyProhibited
  , HostPrecedenceViolation
  , PrecedenceCutoffInEffect
  ]

arbitraryRedirectCode :: Gen RedirectCode
arbitraryRedirectCode  = elements
  [ RedirectForNetwork
  , RedirectForHost
  , RedirectForTypeOfServiceAndNetwork
  , RedirectForTypeOfServiceAndHost
  ]

arbitraryRouterAddress :: Gen RouterAddress
arbitraryRouterAddress  =
  do a <- arbitraryIP4
     b <- arbitraryPreferenceLevel
     return $! RouterAddress a b

arbitraryPreferenceLevel :: Gen PreferenceLevel
arbitraryPreferenceLevel  = arbitraryBoundedRandom

arbitraryTimeExceededCode :: Gen TimeExceededCode
arbitraryTimeExceededCode  = elements
  [ TimeToLiveExceededInTransit
  , FragmentReassemblyTimeExceeded
  ]

arbitraryTraceRouteCode :: Gen TraceRouteCode
arbitraryTraceRouteCode  = elements
  [ TraceRouteForwarded
  , TraceRouteDiscarded
  ]

-- Packet Tests ----------------------------------------------------------------

icmp4Tests :: TestTree
icmp4Tests  = testGroup "Icmp4"
  [ testProperty "Packet encode/decode" $
    encodeDecodeIdentity putIcmp4Packet getIcmp4Packet arbitraryIcmp4Packet
  ]
