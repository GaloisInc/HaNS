{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Hans.Tcp.Tcb (
    -- * Timers
    SlowTicks,
    TcpTimers(..),
    emptyTcpTimers,
    resetRetransmit,
    retryRetransmit,
    stopRetransmit,
    reset2MSL,
    updateTimers,
    calibrateRTO,

    -- * TCB States
    State(..),
    GetState(..),
    whenState,
    setState,

    -- * Sending
    CanSend(..),
    getSndNxt,
    getSndWnd,

    -- * Receiving
    CanReceive(..),
    getRcvNxt,
    getRcvWnd,
    getRcvRight,

    -- * Listening TCBs
    ListenTcb(..),
    newListenTcb,
    createChild,
    reserveSlot, releaseSlot,
    acceptTcb,

    -- * Active TCBs
    Tcb(..),
    newTcb,
    signalDelayedAck,
    setRcvNxt,
    finalizeTcb,
    getSndUna,

    -- ** Active Config
    TcbConfig(..),
    usingTimestamps,
    disableTimestamp,

    -- ** Windowing
    queueBytes,
    haveBytesAvail,
    receiveBytes, tryReceiveBytes,

    -- * TimeWait TCBs
    TimeWaitTcb(..),
    mkTimeWaitTcb,
  ) where

import           Hans.Addr (Addr)
import           Hans.Buffer.Signal
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
import           Data.Int (Int64)
import qualified Data.Sequence as Seq
import           Data.Time.Clock (NominalDiffTime,getCurrentTime)
import           Data.Word (Word16,Word32)
import           MonadLib (BaseM(..))
import           System.CPUTime (getCPUTime)


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
resetRetransmit :: TcpTimers -> (TcpTimers, ())
resetRetransmit TcpTimers { .. } =
  (TcpTimers { ttRetransmitValid = True
             , ttRetransmit      = ttRTO
             , ttRetries         = 0
             , .. }, ())


-- | Increment the retry count, and double the last retransmit timer.
retryRetransmit :: TcpTimers -> (TcpTimers, ())
retryRetransmit TcpTimers { .. } =
  (TcpTimers { ttRetransmitValid = True
             , ttRetransmit      = ttRTO * 2 ^ retries
             , ttRetries         = retries
             , .. }, ())
  where
  retries = ttRetries + 1


-- | Invalidate the retransmit timer.
stopRetransmit :: TcpTimers -> (TcpTimers, ())
stopRetransmit TcpTimers { .. } =
  (TcpTimers { ttRetransmitValid = False
             , ttRetries         = 0
             , .. }, ())


reset2MSL :: Config -> TcpTimers -> (TcpTimers, ())
reset2MSL Config { .. } tt = (tt { tt2MSL = 4 * cfgTcpMSL }, ())
  -- NOTE: cfgTcpMSL is multiplied by four here, as it's given in seconds, but
  -- counted in slow ticks (half seconds). Multiplying by four gives the value
  -- of 2*MSL in slow ticks.


-- | Update all slow-tick timers. Return the old timers, for use with
-- 'atomicModifyIORef\''.
updateTimers :: TcpTimers -> (TcpTimers, TcpTimers)
updateTimers tt = (tt',tt)
  where
  tt' = tt { ttRetransmit = if ttRetransmitValid tt then ttRetransmit tt - 1 else 0
           , tt2MSL       = max 0 (tt2MSL tt - 1)
           , ttIdle       = ttIdle tt + 1
           }


-- | Calibrate the RTO timer, given a round-trip measurement, as specified by
-- RFC-6298.
calibrateRTO :: NominalDiffTime -> TcpTimers -> (TcpTimers, ())
calibrateRTO r tt
  | ttSRTT tt > 0 = (rolling, ())
  | otherwise     = (initial, ())
  where

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
  do old <- atomicModifyIORef' (tcbState tcb) (\old -> (state,old))
     case state of
       Established -> tcbEstablished tcb tcb old
       Closed      -> tcbClosed      tcb tcb old
       CloseWait   -> Stream.closeBuffer (tcbRecvBuffer tcb)
       _           -> return ()

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

data ListenTcb = ListenTcb { lSrc    :: !Addr
                           , lPort   :: !TcpPort

                             -- accept
                           , lAccept       :: !(IORef AcceptQueue)
                           , lAcceptSignal :: !Signal

                           , lTSClock :: !(IORef Send.TSClock)
                           }

-- | Create a new listening socket.
newListenTcb :: Addr -> TcpPort -> Int -> IO ListenTcb
newListenTcb lSrc lPort aqFree =
  do lAccept       <- newIORef (AcceptQueue { aqTcbs = Seq.empty, .. })
     lAcceptSignal <- newSignal

     now      <- getCurrentTime
     tsval    <- getCPUTime
     lTSClock <- newIORef (Send.initialTSClock (fromInteger tsval) now)

     return ListenTcb { .. }


-- | Create a child from a Syn request.
createChild :: HasConfig cfg
            => cfg -> TcpSeqNum -> ListenTcb -> RouteInfo Addr -> Addr -> TcpHeader
            -> (Tcb -> State -> IO ()) -- ^ On Established
            -> (Tcb -> State -> IO ()) -- ^ On Closed
            -> IO Tcb
createChild cxt iss parent ri remote hdr onEstablished onClosed =
  do let cfg = view config cxt

     now <- getCurrentTime
     tsc <- atomicModifyIORef' (lTSClock parent) $ \ tsc ->
                let tsc' = Send.updateTSClock cfg now tsc
                 in (tsc',tsc')

     child <- newTcb cfg (Just parent) iss ri (tcpDestPort hdr) remote
                  (tcpSourcePort hdr) SynReceived tsc
                  (\c state -> do queueTcb parent c state
                                  onEstablished c state)
                  (\c state -> do when (state == SynReceived) (releaseSlot parent)
                                  onClosed c state)


     atomicWriteIORef (tcbIrs child) (tcpSeqNum hdr)
     atomicWriteIORef (tcbIss child)  iss

     -- advance RCV.NXT over the SYN
     _ <- setRcvNxt (tcpSeqNum hdr + 1) child
     _ <- setSndNxt iss child

     return child


data AcceptQueue = AcceptQueue { aqFree :: !Int
                               , aqTcbs :: Seq.Seq Tcb
                               }


-- | Reserve a slot in the accept queue, returns True when the space has been
-- reserved.
reserveSlot :: ListenTcb -> IO Bool
reserveSlot ListenTcb { .. } =
  atomicModifyIORef' lAccept $ \ aq ->
    if aqFree aq > 0
       then (aq { aqFree = aqFree aq - 1 }, True)
       else (aq, False)
{-# INLINE reserveSlot #-}


-- | Release a slot back to the accept queue.
releaseSlot :: ListenTcb -> IO ()
releaseSlot ListenTcb { .. } =
  atomicModifyIORef' lAccept (\ aq -> (aq { aqFree = aqFree aq + 1 }, ()))
{-# INLINE releaseSlot #-}


-- | Add a Tcb to the accept queue for this listening connection.
queueTcb :: ListenTcb -> Tcb -> State -> IO ()
queueTcb ListenTcb { .. } tcb _ =
  do atomicModifyIORef' lAccept $ \ aq ->
         (aq { aqTcbs = aqTcbs aq Seq.|> tcb }, ())
     signal lAcceptSignal


-- | Wait until a Tcb is available in the accept queue.
acceptTcb :: ListenTcb -> IO Tcb
acceptTcb ListenTcb { .. } =
  do waitSignal lAcceptSignal
     atomicModifyIORef' lAccept $ \ AcceptQueue { .. } ->
         case Seq.viewl aqTcbs of

           tcb Seq.:< tcbs ->
             (AcceptQueue { aqTcbs = tcbs, aqFree = aqFree + 1 }, tcb)

           Seq.EmptyL ->
             error "Accept queue signaled with an empty queue"


-- Active Sockets --------------------------------------------------------------

type SeqNumVar = IORef TcpSeqNum

data TcbConfig = TcbConfig { tcUseTimestamp :: !Bool
                           }

defaultTcbConfig :: TcbConfig
defaultTcbConfig  =
  TcbConfig { tcUseTimestamp = True
            }

-- | True when the timestamp option should be included.
usingTimestamps :: Tcb -> IO Bool
usingTimestamps Tcb { .. } =
  do TcbConfig { .. } <- readIORef tcbConfig
     return tcUseTimestamp

-- | Disable the use of the timestamp option.
disableTimestamp :: Tcb -> IO ()
disableTimestamp Tcb { .. } =
  atomicModifyIORef' tcbConfig $ \ TcbConfig { .. } ->
    (TcbConfig { tcUseTimestamp = False, .. }, ())

data Tcb = Tcb { tcbParent :: Maybe ListenTcb
                 -- ^ Parent to notify if this tcb was generated from a socket
                 -- in the LISTEN state

               , tcbConfig :: !(IORef TcbConfig)

               , tcbState       :: !(IORef State)
               , tcbEstablished :: Tcb -> State -> IO ()
               , tcbClosed      :: Tcb -> State -> IO ()

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
               , tcbMss :: !(IORef Int64) -- ^ Maximum segment size

                 -- Timers
               , tcbTimers :: !(IORef TcpTimers)

                 -- Timer option state
               , tcbTSRecent    :: !(IORef Word32)
               , tcbLastAckSent :: !(IORef TcpSeqNum)
               }

newTcb :: HasConfig state
       => state
       -> Maybe ListenTcb
       -> TcpSeqNum -- ^ ISS
       -> RouteInfo Addr -> TcpPort -> Addr -> TcpPort
       -> State
       -> Send.TSClock
       -> (Tcb -> State -> IO ())
       -> (Tcb -> State -> IO ())
       -> IO Tcb
newTcb cxt tcbParent iss tcbRouteInfo tcbLocalPort tcbRemote tcbRemotePort
  state tsc tcbEstablished tcbClosed =
  do let Config { .. } = view config cxt

     tcbConfig <- newIORef defaultTcbConfig

     tcbState  <- newIORef state
     tcbSndUp  <- newIORef 0
     tcbSndWl1 <- newIORef 0
     tcbSndWl2 <- newIORef 0

     tcbSendWindow <-
         newIORef (Send.emptyWindow iss (fromIntegral cfgTcpInitialWindow) tsc)

     tcbIss    <- newIORef iss

     -- NOTE: the size of the receive buffer is gated by the local window
     tcbRecvWindow <- newIORef (Recv.emptyWindow 0 (fromIntegral cfgTcpInitialWindow))
     tcbRecvBuffer <- Stream.newBuffer

     tcbRcvUp  <- newIORef 0
     tcbNeedsDelayedAck <- newIORef False
     tcbIrs    <- newIORef 0
     tcbMss    <- newIORef (fromIntegral cfgTcpInitialMSS)
     tcbTimers <- newIORef emptyTcpTimers

     tcbTSRecent     <- newIORef 0
     tcbLastAckSent  <- newIORef 0

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
finalizeTcb Tcb { .. } =
  do Stream.closeBuffer tcbRecvBuffer
     atomicModifyIORef' tcbTimers stopRetransmit
     atomicModifyIORef' tcbSendWindow Send.flushWindow

-- | Queue bytes in the receive buffer.
queueBytes :: S.ByteString -> Tcb -> IO ()
queueBytes bytes Tcb { .. } = Stream.putBytes bytes tcbRecvBuffer

-- | Determine if there are bytes in the receive buffer that can be read.
haveBytesAvail :: Tcb -> IO Bool
haveBytesAvail Tcb { .. } =
  Stream.bytesAvailable tcbRecvBuffer

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
