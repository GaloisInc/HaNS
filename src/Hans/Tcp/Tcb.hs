{-# LANGUAGE RecordWildCards #-}

module Hans.Tcp.Tcb where

import Hans.Addr (Addr)
import Hans.Lens
import Hans.Network.Types (RouteInfo)
import Hans.Tcp.Packet

import Data.Time.Clock (UTCTime,getCurrentTime,diffUTCTime)
import Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)



-- Socket State ----------------------------------------------------------------

getStateFrom :: GetState tcb => Getting tcb s tcb -> s -> IO State
getStateFrom l s = getState (view l s)

class GetState tcb where
  getState :: tcb -> IO State

instance GetState ListenTcb where
  getState _ = return Listen

instance GetState Tcb where
  getState Tcb { .. } = readIORef tcbState

instance GetState TimeWaitTcb where
  getState TimeWaitTcb { .. } = readIORef twState

data State = Listen
           | SynSent
           | SynReceived
           | Established
           | FinWait1
           | FinWait2
           | CloseWait
           | Closing
           | LastAck
           | TimeWait
           | Closed
             deriving (Eq,Show)


-- Listening Sockets -----------------------------------------------------------

data IssGen = IssGen { issLastSeqNum :: !TcpSeqNum
                     , issLastUpdate :: !UTCTime
                     }

-- | Update the ISS, based on a 128khz incrementing counter.
genIss :: UTCTime -> IssGen -> (IssGen,TcpSeqNum)
genIss now IssGen { .. } = (IssGen iss' now, iss')
  where
  increment = round (diffUTCTime now issLastUpdate * 128000)
  iss'      = issLastSeqNum + increment


data ListenTcb = ListenTcb { lIss  :: !(IORef IssGen)
                           , lSrc  :: !Addr
                           , lPort :: !TcpPort
                           }

nextIss :: ListenTcb -> IO TcpSeqNum
nextIss ListenTcb { .. } =
  do now <- getCurrentTime
     atomicModifyIORef' lIss (genIss now)


-- Active Sockets --------------------------------------------------------------

type SeqNumVar = IORef TcpSeqNum

data Tcb = Tcb { tcbState :: !(IORef State)

                 -- Sender variables
               , tcbSndUna :: !SeqNumVar -- ^ SND.UNA
               , tcbSndNxt :: !SeqNumVar -- ^ SND.NXT
               , tcbSndWnd :: !SeqNumVar -- ^ SND.WND
               , tcbSndUp  :: !SeqNumVar -- ^ SND.UP
               , tcbSndWl1 :: !SeqNumVar -- ^ SND.WL1
               , tcbSndWl2 :: !SeqNumVar -- ^ SND.WL2
               , tcbIss    :: !SeqNumVar -- ^ ISS

                 -- Receive variables
               , tcbRcvNxt :: !SeqNumVar -- ^ RCV.NXT
               , tcbRcvWnd :: !SeqNumVar -- ^ RCV.WND
               , tcbRcvUp  :: !SeqNumVar -- ^ RCV.UP
               , tcbIrs    :: !SeqNumVar -- ^ IRS

                 -- Port information
               , tcbSourcePort :: !TcpPort -- ^ Local port
               , tcbDestPort   :: !TcpPort -- ^ Remote port

                 -- routing information
               , tcbRouteInfo :: !(RouteInfo Addr) -- ^ Cached routing
               , tcbDest      :: !Addr             -- ^ Remote host
               }

newTcb :: RouteInfo Addr -> TcpPort -> Addr -> TcpPort -> State -> IO Tcb
newTcb tcbRouteInfo tcbSourcePort tcbDest tcbDestPort state =
  do tcbState  <- newIORef state
     tcbSndUna <- newIORef 0
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


-- TimeWait Sockets ------------------------------------------------------------

data TimeWaitTcb = TimeWaitTcb { twState      :: !(IORef State)
                               , twSourcePort :: !TcpPort
                               , twDestPort   :: !TcpPort
                               , twRouteInfo  :: !(RouteInfo Addr)
                               , twDest       :: !Addr
                               }
