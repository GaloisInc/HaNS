{-# LANGUAGE RecordWildCards #-}
module Hans.Tcp.Timers where

import           Hans.Config
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Tcp.Tcb
import           Hans.Tcp.Output
import           Hans.Tcp.SendWindow
import           Hans.Time (toUSeconds)
import           Hans.Types

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef (atomicModifyIORef')
import Data.Time.Clock (getCurrentTime,diffUTCTime)


-- | Process the slow and fast tcp timers. The fast timer runs four times a
-- second, while the slow timer runs two times a second.
tcpTimers :: NetworkStack -> IO ()
tcpTimers ns = loop True
  where
  loop runSlow =
    do start <- getCurrentTime

       HT.mapHashTableM_ (\_ -> updateActive ns runSlow) (view tcpActive ns)

       -- delay to the next 250ms boundary
       end <- getCurrentTime
       let delay = 0.250 - diffUTCTime end start
       when (delay > 0) (threadDelay (toUSeconds delay))

       loop $! not runSlow


-- | The body of the fast and slow tick handlers. The boolean indicates whether
-- or not the slow tick should also be run.
updateActive :: NetworkStack -> Bool -> Tcb -> IO ()
updateActive ns runSlow tcb@Tcb { .. } =
  do -- the slow timer
     when runSlow $
       do ts <- atomicModifyIORef' tcbTimers updateTimers
          handleRTO  ns tcb ts
          handle2MSL ns tcb ts

     -- the fast timer
     shouldAck <- atomicModifyIORef' tcbNeedsDelayedAck (\ b -> (False,b))
     when shouldAck (sendAck ns tcb)


-- | Handle the retransmit timer. When the timer expires, if there is anything
-- in the send window, retransmit the left-most segment.
handleRTO :: NetworkStack -> Tcb -> TcpTimers -> IO ()
handleRTO ns Tcb { .. } TcpTimers { .. }
  | ttRetransmitValid && ttRetransmit <= 0 =
    do mbSeg <- atomicModifyIORef' tcbSendWindow retransmitTimeout
       case mbSeg of
         Just (hdr,body) ->
           do atomicModifyIORef' tcbTimers retryRetransmit
              _ <- sendTcp ns tcbRouteInfo tcbRemote hdr body
              return ()

         Nothing ->
              return ()

  | otherwise =
    return ()


-- | Make sure that the connection is still active. The Idle timer is checked
-- when the 2MSL timer expires.
handle2MSL :: NetworkStack -> Tcb -> TcpTimers -> IO ()
handle2MSL ns tcb@Tcb { .. } TcpTimers { .. }

  | tt2MSL <= 0 =
       -- why do we only check the idle timer when 2MSL goes off?
       if ttIdle >= ttMaxIdle
          then closeActive ns tcb
          else atomicModifyIORef' tcbTimers (reset2MSL (view config ns))

  | otherwise =
    return ()
