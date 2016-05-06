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

import           Hans.Addr (NetworkAddr(..),Addr)
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


data Key = Key !Addr !UdpPort
           deriving (Eq,Show,Generic)

instance Hashable Key


type UdpBuffer = DG.Buffer (Device,Addr,UdpPort,Addr,UdpPort)

data UdpState = UdpState { udpRecv  :: !(HT.HashTable Key UdpBuffer)
                         , udpPorts :: !(MVar UdpPort)
                         , udpQueue_:: !(BC.BoundedChan UdpResponderRequest)
                         }

data UdpResponderRequest = SendDatagram !(RouteInfo Addr) !Addr !UdpHeader !L.ByteString


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
           => state -> Addr -> UdpPort -> IO (Maybe UdpBuffer)
lookupRecv state addr dstPort =
  do mb <- HT.lookup (Key addr dstPort) (udpRecv (view udpState state))
     case mb of

       -- there was a receiver waiting on this address and port
       Just _  -> return mb

       -- try the generic receiver for that port
       Nothing -> HT.lookup (Key (wildcardAddr addr) dstPort)
                            (udpRecv (view udpState state))


-- | Register a listener for messages to this address and port, returning 'Just'
-- an action to unregister the listener on success.
registerRecv :: HasUdpState state
             => state -> Addr -> UdpPort -> UdpBuffer -> IO (Maybe (IO ()))
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

nextUdpPort :: HasUdpState state => state -> Addr -> IO (Maybe UdpPort)
nextUdpPort state addr =
  modifyMVar udpPorts (pickFreshPort udpRecv (Key addr))
  where
  UdpState { .. } = view udpState state

pickFreshPort :: HT.HashTable Key UdpBuffer -> (UdpPort -> Key) -> UdpPort
              -> IO (UdpPort, Maybe UdpPort)
pickFreshPort ht mkKey p0 = go 0 p0
  where

  go :: Int -> UdpPort -> IO (UdpPort,Maybe UdpPort)
  go i _ | i > 65535 = return (p0, Nothing)
  go i 0             = go (i+1) 1025
  go i port          =
    do used <- HT.hasKey (mkKey port) ht
       if not used
          then return (port, Just port)
          else go (i + 1) (port + 1)
