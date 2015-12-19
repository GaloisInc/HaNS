{-# LANGUAGE RecordWildCards #-}
module Hans.Tcp.Timers where

import           Hans.Config
import qualified Hans.HashTable as HT
import           Hans.Lens
import           Hans.Tcp.Tcb
import           Hans.Tcp.Output
import           Hans.Time (toUSeconds,partitionExpired)
import           Hans.Types

import Control.Concurrent (threadDelay)
import Control.Monad (when,unless)
import Data.IORef (atomicModifyIORef',readIORef)
import Data.Time.Clock (UTCTime,getCurrentTime,diffUTCTime)


-- | Process the slow and fast tcp timers. The fast timer runs four times a
-- second, while the slow timer runs two times a second.
tcpTimers :: NetworkStack -> IO ()
tcpTimers ns = loop True
  where
  loop runSlow =
    do start <- getCurrentTime

       when runSlow (reapTimeWait ns start)
       HT.mapHashTableM_ (updateActive runSlow) (view tcpActive ns)

       -- delay to the next 250ms boundary
       end <- getCurrentTime
       let delay = 0.250 - diffUTCTime end start
       unless (delay < 0) (threadDelay (toUSeconds delay))

       loop (not runSlow)


-- | The body of the fast and slow tick handlers. The boolean indicates whether
-- or not the slow tick should also be run.
updateActive :: Bool -> Tcb -> IO ()
updateActive runSlow tcb@Tcb { .. } =
  do -- the slow timer
     when runSlow $
       do handleRTO tcb
          handle2MSL tcb
          atomicModifyIORef' (tcbTimers tcb) updateTimers

     -- the fast timer
     shouldAck <- atomicModifyIORef' (tcbNeedsDelayedAck tcb) (\ b -> (False,b))
     when shouldAck (sendAck tcb)


-- | Handle the retransmit timer.
handleRTO :: Tcb -> IO ()
handleRTO Tcb { .. } =
  putStrLn "handleRTO"


-- | Make sure that the connection is still active. The Idle timer is checked
-- when the 2MSL timer expires.
handle2MSL :: NetworkStack -> Tcb -> IO ()
handle2MSL ns tcb@Tcb { .. } =
  do TcpTimers { .. } <- readIORef tcbTimers
     when (tt2MSL == 0) $
       -- why do we only check the idle timer when 2MSL goes off?
       if ttIdle >= ttMaxIdle
          then closeActive tcb
          else atomicModifyIORef' tcbTimers (reset2MSL (view config ns))


sendAck :: Tcb -> IO ()
sendAck  = undefined


-- | Remove entries from the TIME_WAIT heap that have expired.
reapTimeWait :: NetworkStack -> UTCTime -> IO ()
reapTimeWait ns now =
  do _expired <- atomicModifyIORef' (view tcpTimeWait ns) (partitionExpired now)
     return ()


-- | Step the timers by one slow tick: decrement 2MSL, and increment the idle
-- timer.
updateTimers :: TcpTimers -> (TcpTimers, ())
updateTimers tt = (tt',())
  where
  tt' = tt { tt2MSL = max 0 (tt2MSL tt - 1)
           , ttIdle = ttIdle tt + 1 }


-- | Reset the 2MSL timer.
reset2MSL :: Config -> TcpTimers -> (TcpTimers, ())
reset2MSL Config { .. } tt = (tt { tt2MSL = 4 * cfgTcpMSL }, ())
  -- NOTE: cfgTcpMSL is multiplied by four here, as it's given in seconds, but
  -- counted in slow ticks (half seconds). Multiplying by four gives the value
  -- of 2*MSL in slow ticks.
