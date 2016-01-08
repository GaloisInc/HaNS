{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Hans.Tcp.Tcb where

import           Hans.Addr (Addr)
import qualified Hans.Buffer.Stream as Stream
import           Hans.Config (HasConfig(..),Config(..))
import           Hans.Lens
import           Hans.Network.Types (RouteInfo)
import           Hans.Tcp.Packet
import qualified Hans.Tcp.RecvWindow as Recv
import qualified Hans.Tcp.SendWindow as Send

import           Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef
                     (IORef,newIORef,atomicModifyIORef',readIORef
                     ,atomicWriteIORef)
import           Data.Time.Clock
                     (UTCTime,getCurrentTime,diffUTCTime,NominalDiffTime)
import           Data.Word (Word32,Word16)
import           MonadLib (BaseM(..))


-- Timers ----------------------------------------------------------------------

type SlowTicks = Int

data TcpTimers = TcpTimers { ttDelayedAck :: !Bool

                             -- MSL
                           , tt2MSL :: !SlowTicks

                             -- retransmit timer
                           , ttRetransmitValid :: !Bool
                           , ttRetransmit      :: !SlowTicks
                           , ttRetries         :: !Int

                             -- retransmit timer config
                           , ttRTO    :: !SlowTicks
                           , ttSRTT   :: !NominalDiffTime
                           , ttRTTVar :: !NominalDiffTime

                             -- idle timer
                           , ttMaxIdle :: !SlowTicks
                           , ttIdle    :: !SlowTicks
                           }

emptyTcpTimers :: TcpTimers
emptyTcpTimers  = TcpTimers { ttDelayedAck = False
                            , tt2MSL       = 0

                            , ttRetransmitValid = False
                            , ttRetransmit      = 0
                            , ttRetries         = 0

                            , ttRTO        = 2 -- two ticks -- one second
                            , ttSRTT       = 0
                            , ttRTTVar     = 0
                            , ttMaxIdle    = 10 * 60 * 2 -- 10 minutes
                            , ttIdle       = 0
                            }


-- | Reset retransmit info.
resetRetransmit :: TcpTimers -> TcpTimers
resetRetransmit TcpTimers { .. } =
  TcpTimers { ttRetransmitValid = True
            , ttRetransmit      = ttRTO
            , ttRetries         = 0
            , .. }


-- | Increment the retry count, and double the last retransmit timer.
retryRetransmit :: TcpTimers -> TcpTimers
retryRetransmit TcpTimers { .. } =
  TcpTimers { ttRetransmitValid = True
            , ttRetransmit      = ttRTO * 2 ^ retries
            , ttRetries         = retries
            , .. }
  where
  retries = ttRetries + 1


-- | Invalidate the retransmit timer.
stopRetransmit :: TcpTimers -> TcpTimers
stopRetransmit TcpTimers { .. } =
  TcpTimers { ttRetransmitValid = False
            , ttRetries         = 0
            , .. }


reset2MSL :: Config -> TcpTimers -> (TcpTimers, ())
reset2MSL Config { .. } tt = (tt { tt2MSL = 4 * cfgTcpMSL }, ())
  -- NOTE: cfgTcpMSL is multiplied by four here, as it's given in seconds, but
  -- counted in slow ticks (half seconds). Multiplying by four gives the value
  -- of 2*MSL in slow ticks.


-- | Update all slow-tick timers. Return the old timers, for use with
-- 'atomicModifyIORef'.
updateTimers :: TcpTimers -> (TcpTimers, TcpTimers)
updateTimers tt = (tt',tt)
  where
  tt' = tt { ttRetransmit = if ttRetransmitValid tt then ttRetransmit tt - 1 else 0
           , tt2MSL       = max 0 (tt2MSL tt - 1)
           , ttIdle       = ttIdle tt + 1 }


-- | Calibrate the RTO timer, as specified by RFC-6298.
calibrateRTO :: UTCTime -> UTCTime -> TcpTimers -> TcpTimers
calibrateRTO sent ackd tt
  | ttSRTT tt > 0 = rolling
  | otherwise     = initial
  where

  -- round trip measurement
  r = diffUTCTime ackd sent

  -- no data has been sent before, seed the RTO values.
  initial = updateRTO tt
    { ttSRTT   = r
    , ttRTTVar = r / 2
    }

  -- data has been sent, update based on previous values
  alpha   = 0.125
  beta    = 0.25
  rttvar  = (1 - beta)  * ttRTTVar tt + beta  * abs (ttSRTT tt * r)
  srtt    = (1 - alpha) * ttSRTT   tt + alpha * r
  rolling = updateRTO tt
    { ttRTTVar = rttvar
    , ttSRTT   = srtt
    }

  -- update the RTO timer length, bounding it at 64 seconds (128 ticks)
  updateRTO tt' = tt'
    { ttRTO = min 128 (ceiling (ttSRTT tt' + max 0.5 (2 * ttRTTVar tt')))
    }


-- Socket State ----------------------------------------------------------------

whenState :: (BaseM m IO, GetState tcb) => tcb -> State -> m () -> m ()
whenState tcb state m =
  do state' <- inBase (getState tcb)
     when (state == state') m

-- | The Tcb type is the only one that supports changing state.
setState :: Tcb -> State -> IO ()
setState tcb state =
  do atomicWriteIORef (tcbState tcb) state
     case state of
       Established -> tcbEstablished tcb tcb
       Closed      -> tcbClosed      tcb tcb

       -- unblock the recv queue, so that the user can close the socket
       CloseWait   -> Stream.putBytes S.empty (tcbRecvBuffer tcb)

       _           -> return ()

getStateFrom :: GetState tcb => Getting tcb s tcb -> s -> IO State
getStateFrom l s = getState (view l s)

class GetState tcb where
  getState :: tcb -> IO State

instance GetState ListenTcb where
  getState _ = return Listen

instance GetState Tcb where
  getState Tcb { .. } = readIORef tcbState

instance GetState TimeWaitTcb where
  getState _ = return TimeWait

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
  increment :: Word32
  increment  = round (diffUTCTime now issLastUpdate * 128000)
  iss'       = issLastSeqNum + fromIntegral increment


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

data Tcb = Tcb { tcbParent :: Maybe ListenTcb
                 -- ^ Parent to notify if this tcb was generated from a socket
                 -- in the LISTEN state

               , tcbState       :: !(IORef State)
               , tcbEstablished :: Tcb -> IO ()
               , tcbClosed      :: Tcb -> IO ()

                 -- Sender variables
               , tcbSndUp  :: !SeqNumVar -- ^ SND.UP
               , tcbSndWl1 :: !SeqNumVar -- ^ SND.WL1
               , tcbSndWl2 :: !SeqNumVar -- ^ SND.WL2
               , tcbIss    :: !SeqNumVar -- ^ ISS
               , tcbSendWindow :: !(IORef Send.Window)

                 -- Receive variables
               , tcbRcvUp  :: !SeqNumVar -- ^ RCV.UP
               , tcbIrs    :: !SeqNumVar -- ^ IRS

               , tcbNeedsDelayedAck :: !(IORef Bool)
               , tcbRecvWindow :: !(IORef Recv.Window)
               , tcbRecvBuffer :: !Stream.Buffer

                 -- Port information
               , tcbLocalPort  :: !TcpPort -- ^ Local port
               , tcbRemotePort :: !TcpPort -- ^ Remote port

                 -- Routing information
               , tcbRouteInfo :: !(RouteInfo Addr) -- ^ Cached routing
               , tcbRemote    :: !Addr             -- ^ Remote host

                 -- Fragmentation information
               , tcbMss :: !(IORef Int) -- ^ Maximum segment size

                 -- Timers
               , tcbTimers :: !(IORef TcpTimers)
               }

newTcb :: HasConfig state
       => state
       -> Maybe ListenTcb
       -> TcpSeqNum -- ^ ISS
       -> RouteInfo Addr -> TcpPort -> Addr -> TcpPort
       -> State
       -> (Tcb -> IO ())
       -> (Tcb -> IO ())
       -> IO Tcb
newTcb cxt tcbParent iss tcbRouteInfo tcbLocalPort tcbRemote tcbRemotePort state
  tcbEstablished tcbClosed =
  do let Config { .. } = view config cxt
     tcbState  <- newIORef state
     tcbSndUp  <- newIORef 0
     tcbSndWl1 <- newIORef 0
     tcbSndWl2 <- newIORef 0

     tcbSendWindow <-
         newIORef (Send.emptyWindow iss (fromIntegral cfgTcpInitialWindow))

     tcbIss    <- newIORef iss

     -- NOTE: the size of the receive buffer is gated by the local window
     tcbRecvWindow <- newIORef (Recv.emptyWindow 0 (fromIntegral cfgTcpInitialWindow))
     tcbRecvBuffer <- Stream.newBuffer

     tcbRcvUp  <- newIORef 0
     tcbNeedsDelayedAck <- newIORef False
     tcbIrs    <- newIORef 0
     tcbMss    <- newIORef cfgTcpInitialMSS
     tcbTimers <- newIORef emptyTcpTimers
     return Tcb { .. }

-- | Record that a delayed ack should be sent.
signalDelayedAck :: Tcb -> IO ()
signalDelayedAck Tcb { .. } = atomicWriteIORef tcbNeedsDelayedAck True

-- | Set the value of RCV.NXT. Returns 'True' when the value has been set
-- successfully, and 'False' if the receive queue was not empty.
setRcvNxt :: TcpSeqNum -> Tcb -> IO Bool
setRcvNxt rcvNxt Tcb { .. } =
  atomicModifyIORef' tcbRecvWindow (Recv.setRcvNxt rcvNxt)


-- | Set the value of SND.NXT. Returns 'True' when the value has been set
-- successfully, and 'False' if the send queue was not empty.
setSndNxt :: TcpSeqNum -> Tcb -> IO Bool
setSndNxt sndNxt Tcb { .. } =
  atomicModifyIORef' tcbSendWindow (Send.setSndNxt sndNxt)


-- | Cleanup the Tcb.
finalizeTcb :: Tcb -> IO ()
finalizeTcb Tcb { .. } = undefined

-- | Queue bytes in the receive buffer.
queueBytes :: S.ByteString -> Tcb -> IO ()
queueBytes bytes Tcb { .. } = Stream.putBytes bytes tcbRecvBuffer

-- | Remove data from the receive buffer, and move the right-side of the receive
-- window. Reading 0 bytes indicates that the remote side has closed the
-- connection.
receiveBytes :: Int -> Tcb -> IO L.ByteString
receiveBytes len Tcb { .. } =
  do bytes <- Stream.takeBytes len tcbRecvBuffer
     atomicModifyIORef' tcbRecvWindow
         (Recv.moveRcvRight (fromIntegral (L.length bytes)))
     return bytes

-- | Non-blocking version of 'receiveBytes'. Reading 0 bytes indicates that the
-- remote side has closed the connection.
tryReceiveBytes :: Int -> Tcb -> IO (Maybe L.ByteString)
tryReceiveBytes len Tcb { .. } =
  do mbBytes <- Stream.tryTakeBytes len tcbRecvBuffer
     case mbBytes of

       Just bytes ->
         do atomicModifyIORef' tcbRecvWindow
                (Recv.moveRcvRight (fromIntegral (L.length bytes)))
            return (Just bytes)

       Nothing ->
            return Nothing


-- TimeWait Sockets ------------------------------------------------------------

data TimeWaitTcb = TimeWaitTcb { twSndNxt     :: !TcpSeqNum     -- ^ SND.NXT

                               , twRcvNxt     :: !SeqNumVar     -- ^ RCV.NXT
                               , twRcvWnd     :: !Word16        -- ^ RCV.WND

                                 -- Port information
                               , twLocalPort  :: !TcpPort
                               , twRemotePort :: !TcpPort

                                 -- Routing information
                               , twRouteInfo  :: !(RouteInfo Addr)
                               , twRemote     :: !Addr
                               } deriving (Eq)

mkTimeWaitTcb :: Tcb -> IO TimeWaitTcb
mkTimeWaitTcb Tcb { .. } =
  do send     <- readIORef tcbSendWindow
     recv     <- readIORef tcbRecvWindow
     twRcvNxt <- newIORef (view Recv.rcvNxt recv)

     return $! TimeWaitTcb { twSndNxt     = view Send.sndNxt send
                           , twRcvWnd     = view Recv.rcvWnd recv
                           , twLocalPort  = tcbLocalPort
                           , twRemotePort = tcbRemotePort
                           , twRouteInfo  = tcbRouteInfo
                           , twRemote     = tcbRemote
                           , .. }



-- Sockets that send -----------------------------------------------------------

getSndNxt :: (BaseM io IO, CanSend sock) => sock -> io TcpSeqNum
getSndNxt sock =
  do (nxt,_) <- getSendWindow sock
     return nxt
{-# INLINE getSndNxt #-}

getSndWnd :: (BaseM io IO, CanSend sock) => sock -> io TcpSeqNum
getSndWnd sock =
  do (_,wnd) <- getSendWindow sock
     return wnd
{-# INLINE getSndWnd #-}

class CanSend sock where
  getSendWindow :: BaseM io IO => sock -> io (TcpSeqNum,TcpSeqNum)

instance CanSend (IORef Send.Window) where
  getSendWindow ref =
    do sw <- inBase (readIORef ref)
       return (view Send.sndNxt sw, view Send.sndWnd sw)
  {-# INLINE getSendWindow #-}

instance CanSend Tcb where
  getSendWindow Tcb { .. } = getSendWindow tcbSendWindow
  {-# INLINE getSendWindow #-}


-- Sockets that receive --------------------------------------------------------

getSndUna :: BaseM io IO => Tcb -> io TcpSeqNum
getSndUna Tcb { .. } =
  do sw <- inBase (readIORef tcbSendWindow)
     return $! view Send.sndUna sw

getRcvNxt :: (BaseM io IO, CanReceive sock) => sock -> io TcpSeqNum
getRcvNxt sock =
  do (nxt,_) <- getRecvWindow sock
     return nxt
{-# INLINE getRcvNxt #-}

getRcvWnd :: (BaseM io IO, CanReceive sock) => sock -> io Word16
getRcvWnd sock =
  do (nxt,right) <- getRecvWindow sock
     return (fromTcpSeqNum (right - nxt))
{-# INLINE getRcvWnd #-}

getRcvRight :: (BaseM io IO, CanReceive sock) => sock -> io TcpSeqNum
getRcvRight sock =
  do (_,right) <- getRecvWindow sock
     return right
{-# INLINE getRcvRight #-}

class CanReceive sock where
  -- | Retrieve the left and right edges of the receive window:
  --
  -- (RCV.NXT, RCV.NXT + RCV.WND)
  getRecvWindow :: BaseM io IO => sock -> io (TcpSeqNum,TcpSeqNum)

instance CanReceive (IORef Recv.Window) where
  getRecvWindow ref = inBase $
    do rw <- readIORef ref
       return (view Recv.rcvNxt rw, view Recv.rcvRight rw)
  {-# INLINE getRecvWindow #-}

instance CanReceive Tcb where
  getRecvWindow Tcb { .. } = getRecvWindow tcbRecvWindow
  {-# INLINE getRecvWindow #-}

instance CanReceive TimeWaitTcb where
  getRecvWindow TimeWaitTcb { .. } = inBase $
    do rcvNxt <- readIORef twRcvNxt
       return (rcvNxt,rcvNxt + fromIntegral twRcvWnd)
  {-# INLINE getRecvWindow #-}
