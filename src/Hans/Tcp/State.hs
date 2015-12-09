{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Hans.Tcp.State where

import           Hans.Config (Config(..))
import           Hans.Device.Types (Device)
import           Hans.IP4.Packet (IP4,pattern WildcardIP4)
import           Hans.IP4.State (HasIP4State,lookupRoute)
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Tcp.Packet

import           Data.Hashable (Hashable)
import           Data.IORef
                     (IORef,newIORef,atomicModifyIORef',atomicWriteIORef)
import           Data.Time.Clock (getCurrentTime,UTCTime,diffUTCTime)
import           GHC.Generics (Generic)


-- General State ---------------------------------------------------------------

data Key = Listen4 !IP4 !TcpPort
           -- ^ Listening connections

         | Conn4 !IP4 !TcpPort !IP4 !TcpPort
           -- ^ Open connections

           deriving (Show,Eq,Ord,Generic)

instance Hashable Key


data TcpState =
  TcpState { tcpSockets :: {-# UNPACK #-} !(HT.HashTable Key TcbState)
           }


class HasTcpState state where
  tcpState :: Getting r state TcpState

instance HasTcpState TcpState where
  tcpState = id
  {-# INLINE tcpState #-}


newTcpState :: Config -> IO TcpState
newTcpState Config { .. } =
  do tcpSockets <- HT.newHashTable cfgTcpSocketTableSize
     return TcpState { .. }


-- Tcb -------------------------------------------------------------------------

data TcbState = Listen !ListenTcb
              | Established !Tcb
              | Closed


data IssGen = IssGen { issLastSeqNum :: !TcpSeqNum
                     , issLastUpdate :: !UTCTime
                     }

-- | Update the ISS, based on a 128khz incrementing counter.
genIss :: UTCTime -> IssGen -> (IssGen,TcpSeqNum)
genIss now IssGen { .. } = (IssGen iss' now, iss')
  where
  increment = round (diffUTCTime now issLastUpdate * 128000)
  iss'      = issLastSeqNum + increment


data ListenTcb = ListenTcb { ltcbIss :: !(IORef IssGen)
                           }

nextIss :: ListenTcb -> IO TcpSeqNum
nextIss ListenTcb { .. } =
  do now <- getCurrentTime
     atomicModifyIORef' ltcbIss (genIss now)


data Tcb = Tcb { tcbSndUna
               , tcbSndNxt
               , tcbSndWnd
               , tcbSndUp
               , tcbSndWl1
               , tcbSndWl2
               , tcbIss
               , tcbRcvNxt
               , tcbRcvWnd
               , tcbRcvUp
               , tcbIrs :: !(IORef TcpSeqNum)

                 -- routing information
               , tcbDev   :: !Device
               , tcbSrc4  :: !IP4
               , tcbDst4  :: !IP4
               , tcbNext4 :: !IP4
               }


newTcb :: Device -> IP4 -> IP4 -> IP4 -> IO Tcb
newTcb tcbDev tcbSrc4 tcbDst4 tcbNext4 =
  do tcbSndUna <- newIORef 0
     tcbSndNxt <- newIORef 0
     tcbSndWnd <- newIORef 0
     tcbSndUp  <- newIORef 0
     tcbSndWl1 <- newIORef 0
     tcbSndWl2 <- newIORef 0
     tcbIss    <- newIORef 0
     tcbRcvNxt <- newIORef 0
     tcbRcvWnd <- newIORef 0
     tcbRcvUp  <- newIORef 0
     tcbIrs    <- newIORef 0
     return Tcb { .. }


addCounter :: (IORef TcpSeqNum) -> TcpSeqNum -> IO ()
addCounter ref n = atomicModifyIORef' ref (\ c -> (c + n, ()))


-- | Lookup the Tcb for an established connection.
findEstablished4 :: HasTcpState state
                 => state -> IP4 -> TcpPort -> IP4 -> TcpPort -> IO (Maybe TcbState)
findEstablished4 state src srcPort dst dstPort =
  HT.lookup (Conn4 src srcPort dst dstPort) (tcpSockets (view tcpState state))


-- | Find the Tcb for a listening connection.
findListening4 :: HasTcpState state => state -> IP4 -> TcpPort -> IO (Maybe TcbState)
findListening4 state src srcPort =
  do mb <- HT.lookup (Listen4 src srcPort) tcpSockets
     case mb of
       Just{}  -> return mb
       Nothing -> HT.lookup (Listen4 WildcardIP4 srcPort) tcpSockets

  where
  TcpState { .. } = view tcpState state


-- | Find a Tcb.
findTcb4 :: HasTcpState state
         => state -> IP4 -> TcpPort -> IP4 -> TcpPort -> IO TcbState
findTcb4 state src srcPort dst dstPort =
  do mb <- findEstablished4 state src srcPort dst dstPort
     case mb of
       Just tcb -> return tcb
       Nothing  -> do mb' <- findListening4 state dst dstPort
                      case mb' of
                        Just tcb -> return tcb
                        Nothing  -> return Closed



-- | Generate a Tcb from a ListenTcb
fromListen :: HasIP4State state
           => state -> IP4 -> TcpPort -> IP4 -> TcpPort -> ListenTcb
           -> IO (Maybe Tcb)
fromListen state src srcPort dst dstPort ltcb =
  do mb <- lookupRoute state dst
     case mb of
       Just (src',next,dev) | src == src' ->
         do tcb <- newTcb dev src' dst next
            iss <- nextIss ltcb
            atomicWriteIORef (tcbIss tcb) $! iss

            return (Just tcb)

       Nothing -> return Nothing
