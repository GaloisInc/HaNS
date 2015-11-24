{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Hans.Udp.State (
    UdpState(..), newUdpState,
    HasUdpState(..),
    UdpBuffer,
    lookupRecv4, Receiver(..),
    registerRecv4,
  ) where

import qualified Hans.Buffer.Datagram as DG
import           Hans.Config
import           Hans.Device.Types (Device)
import qualified Hans.HashTable as HT
import           Hans.IP4.Packet (IP4, pattern CurrentNetworkIP4)
import           Hans.Udp.Packet (UdpPort)

import           Data.Hashable (Hashable)
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import           GHC.Generics (Generic)


data Key = Key4 !IP4 !UdpPort
           deriving (Eq,Show,Generic)

instance Hashable Key


type UdpBuffer addr = DG.Buffer (Device,addr,UdpPort,addr,UdpPort)

data Receiver = Receiver4 !(UdpBuffer IP4)

data UdpState = UdpState { udpRecv :: HT.HashTable Key Receiver
                         }

newUdpState :: Config -> IO UdpState
newUdpState Config { .. } =
  do udpRecv <- HT.newHashTable cfgUdpSocketTableSize
     return $! UdpState { .. }


class HasUdpState udp where
  getUdpState :: udp -> UdpState

instance HasUdpState UdpState where
  getUdpState = id
  {-# INLINE getUdpState #-}

lookupRecv4 :: HasUdpState state => state -> IP4 -> UdpPort -> IO (Maybe Receiver)
lookupRecv4 state dst dstPort =
  do mb <- HT.lookup (Key4 dst dstPort) (udpRecv (getUdpState state))
     case mb of

       -- there was a receiver waiting on this address and port
       Just _  -> return mb

       -- try the generic receiver for that port
       Nothing -> HT.lookup (Key4 CurrentNetworkIP4 dstPort) (udpRecv (getUdpState state))


-- | Register a listener for messages to this address and port, returning 'Just'
-- an action to unregister the listener on success.
registerRecv4 :: HasUdpState state
              => state -> IP4 -> UdpPort -> UdpBuffer IP4 -> IO (Maybe (IO ()))
registerRecv4 state src srcPort buf =
  do registered <- HT.alter update key table
     if registered
        then return (Just (HT.delete key table))
        else return Nothing
  where
  table = udpRecv (getUdpState state)

  key = Key4 src srcPort

  update mb@Just{} = (mb,False)
  update Nothing   = (Just (Receiver4 buf),True)
