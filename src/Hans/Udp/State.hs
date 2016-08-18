{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Hans.Udp.State (
    UdpState(..), newUdpState,
    HasUdpState(..),
    UdpBuffer,
    lookupRecv,
    registerRecv,
    nextUdpPort,

    -- ** Fast-path Resonder
    UdpResponderRequest(..),
    udpQueue,
  ) where

import           Hans.Addr (IP6,wildcardAddr,toIP4,toIP6,isWildcard)
import qualified Hans.Buffer.Datagram as DG
import           Hans.Config
import           Hans.Device.Types (Device)
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Network.Types (RouteInfo)
import           Hans.Udp.Packet (UdpPort,UdpHeader)

import           Control.Concurrent (MVar,newMVar,modifyMVar)
import qualified Control.Concurrent.BoundedChan as BC
import qualified Data.ByteString.Lazy as L
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)


data Key = Key !IP6 !UdpPort
           deriving (Eq,Show,Generic)

instance Hashable Key


type UdpBuffer = DG.Buffer (Device,IP6,UdpPort,IP6,UdpPort)

data UdpState = UdpState { udpRecv  :: !(HT.HashTable Key UdpBuffer)
                         , udpPorts :: !(MVar UdpPort)
                         , udpQueue_:: !(BC.BoundedChan UdpResponderRequest)
                         }

data UdpResponderRequest = SendDatagram !(RouteInfo IP6) !IP6 !UdpHeader !L.ByteString


newUdpState :: Config -> IO UdpState
newUdpState Config { .. } =
  do udpRecv  <- HT.newHashTable cfgUdpSocketTableSize
     udpPorts <- newMVar 32767
     udpQueue_<- BC.newBoundedChan 128
     return $! UdpState { .. }


class HasUdpState udp where
  udpState :: Getting r udp UdpState

instance HasUdpState UdpState where
  udpState = id
  {-# INLINE udpState #-}

udpQueue :: HasUdpState state => Getting r state (BC.BoundedChan UdpResponderRequest)
udpQueue  = udpState . to udpQueue_

lookupRecv :: HasUdpState state
           => state -> IP6 -> UdpPort -> IO (Maybe UdpBuffer)
lookupRecv state addr dstPort =
  do mb <- HT.lookup (Key addr dstPort) (udpRecv (view udpState state))
     case mb of

       -- there was a receiver waiting on this address and port
       Just _  -> return mb

       -- try the generic receiver for that port
       Nothing ->
         case toIP4 addr of

           Just ip4 ->
             do mb' <- HT.lookup (Key (toIP6 (wildcardAddr ip4)) dstPort)
                                 (udpRecv (view udpState state))
                case mb' of
                  Just{}  -> return mb'
                  Nothing -> HT.lookup (Key (wildcardAddr addr) dstPort)
                                       (udpRecv (view udpState state))

           Nothing -> HT.lookup (Key (wildcardAddr addr) dstPort)
                                (udpRecv (view udpState state))


-- | Register a listener for messages to this address and port, returning 'Just'
-- an action to unregister the listener on success.
registerRecv :: HasUdpState state
             => state -> IP6 -> UdpPort -> UdpBuffer -> IO (Maybe (IO ()))
registerRecv state addr srcPort buf =
  do registered <- HT.alter update key table
     if registered
        then return (Just (HT.delete key table))
        else return Nothing
  where
  table = udpRecv (view udpState state)

  key = Key addr srcPort

  update mb@Just{} = (mb,False)
  update Nothing   = (Just buf,True)


-- Port Management -------------------------------------------------------------

nextUdpPort :: HasUdpState state => state -> IP6 -> IO (Maybe UdpPort)
nextUdpPort state addr =
  modifyMVar udpPorts (pickFreshPort udpRecv addr)
  where
  UdpState { .. } = view udpState state

pickFreshPort :: HT.HashTable Key UdpBuffer -> IP6 -> UdpPort
              -> IO (UdpPort, Maybe UdpPort)
pickFreshPort ht addr p0 = go 0 p0
  where

  mkKey1 = Key addr
  mkKey2 = Key (wildcardAddr addr)

  check
    | isWildcard addr = \port ->
         HT.hasKey (mkKey1 port) ht

    | Just ip4 <- toIP4 addr, isWildcard ip4 = \port ->
         HT.hasKey (mkKey1 port) ht

    | otherwise = \port ->
      do used <- HT.hasKey (mkKey1 port) ht
         if not used
            then HT.hasKey (mkKey2 port) ht
            else return True

  go :: Int -> UdpPort -> IO (UdpPort,Maybe UdpPort)
  go i _ | i > 65535 = return (p0, Nothing)
  go i 0             = go (i+1) 1025
  go i port          =
    do used <- check port
       if not used
          then return (port, Just port)
          else go (i + 1) (port + 1)
