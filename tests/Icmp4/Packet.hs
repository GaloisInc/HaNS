module Icmp4.Packet where

import IP4.Addr
import Utils

import Hans.Message.Icmp4
import Hans.Message.Types (Lifetime(..))

import Control.Applicative ((<$>),(<*>),pure)
import System.Random ()
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
    (Gen,oneof,arbitraryBoundedRandom,elements,listOf,listOf1,vectorOf)
import qualified Data.ByteString as S


-- Packet Generation -----------------------------------------------------------

arbitraryIcmp4Packet :: Gen Icmp4Packet
arbitraryIcmp4Packet  = oneof
  [ EchoReply <$> arbitraryIdentifier
              <*> arbitrarySequenceNumber
              <*> arbitraryEchoPayload
  , DestinationUnreachable <$> arbitraryDestinationUnreachableCode
                           <*> arbitraryPayload
  , SourceQuench <$> arbitraryPayload
  , Redirect <$> arbitraryRedirectCode
             <*> arbitraryIP4
             <*> arbitraryPayload
  , Echo <$> arbitraryIdentifier
         <*> arbitrarySequenceNumber
         <*> arbitraryEchoPayload
  , RouterAdvertisement <$> arbitraryLifetime
                        <*> listOf1 arbitraryRouterAddress
  , pure RouterSolicitation
  , TimeExceeded <$> arbitraryTimeExceededCode
                 <*> arbitraryPayload
  , ParameterProblem <$> arbitraryBoundedRandom
                     <*> arbitraryPayload
  , Timestamp <$> arbitraryIdentifier
              <*> arbitrarySequenceNumber
              <*> arbitraryBoundedRandom
              <*> arbitraryBoundedRandom
              <*> arbitraryBoundedRandom
  , TimestampReply <$> arbitraryIdentifier
                   <*> arbitrarySequenceNumber
                   <*> arbitraryBoundedRandom
                   <*> arbitraryBoundedRandom
                   <*> arbitraryBoundedRandom
  , Information <$> arbitraryIdentifier
                <*> arbitrarySequenceNumber
  , InformationReply <$> arbitraryIdentifier
                     <*> arbitrarySequenceNumber
  , TraceRoute <$> arbitraryTraceRouteCode
               <*> arbitraryIdentifier
               <*> arbitraryBoundedRandom
               <*> arbitraryBoundedRandom
               <*> arbitraryBoundedRandom
               <*> arbitraryBoundedRandom
  , AddressMask <$> arbitraryIdentifier
                <*> arbitrarySequenceNumber
  , AddressMaskReply <$> arbitraryIdentifier
                     <*> arbitrarySequenceNumber
                     <*> arbitraryBoundedRandom
  ]

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier  = Identifier <$> arbitraryBoundedRandom

arbitrarySequenceNumber :: Gen SequenceNumber
arbitrarySequenceNumber  = SequenceNumber <$> arbitraryBoundedRandom

arbitraryEchoPayload :: Gen S.ByteString
arbitraryEchoPayload  = S.pack <$> listOf arbitraryBoundedRandom

arbitraryPayload :: Gen S.ByteString
arbitraryPayload  = S.pack <$> vectorOf 28 arbitraryBoundedRandom

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

arbitraryLifetime :: Gen Lifetime
arbitraryLifetime  = Lifetime <$> arbitraryBoundedRandom

arbitraryRouterAddress :: Gen RouterAddress
arbitraryRouterAddress  = RouterAddress
                      <$> arbitraryIP4
                      <*> arbitraryPreferenceLevel

arbitraryPreferenceLevel :: Gen PreferenceLevel
arbitraryPreferenceLevel  = PreferenceLevel <$> arbitraryBoundedRandom

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

icmp4PacketTests :: Test
icmp4PacketTests  = testGroup "packet parsing"
  [ testProperty "roundTrip" prop_roundTrip
  ]

prop_roundTrip =
  roundTrip arbitraryIcmp4Packet getIcmp4Packet putIcmp4Packet
