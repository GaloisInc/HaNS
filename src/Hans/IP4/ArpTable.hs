{-# LANGUAGE RecordWildCards #-}

module Hans.IP4.ArpTable (
    -- * Arp Table
    ArpTable(), newArpTable,

    -- ** Update
    addEntry,
    markUnreachable,

    -- ** Query
    lookupEntry,
    resolveAddr, QueryResult(..),
    WaitStrategy(), blockingStrategy, writeChanStrategy

  ) where

import           Hans.Addr (IP4)
import           Hans.Config (Config(..))
import           Hans.Device.Types (DeviceStats,updateError,statTX)
import           Hans.Ethernet (Mac)
import qualified Hans.HashTable as HT
import           Hans.Threads (forkNamed)
import           Hans.Time (toUSeconds)

import           Control.Concurrent
                     (threadDelay,MVar,newEmptyMVar,ThreadId,tryPutMVar)
import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad (forever)
import           Data.Time.Clock
                     (UTCTime,NominalDiffTime,addUTCTime,getCurrentTime)


-- | The Arp table consists of a map of IP4 to Mac, as well as a heap that
-- orders the IP4 addresses according to when their corresponding entries should
-- be expired.
--
-- NOTE: There's currently no way to limit the memory use of the arp table, but
-- that might not be a huge problem as the entries are added based on requests
-- from higher layers.
--
-- INVARIANT: there should never be entries in the map that aren't also in the
-- heap.
data ArpTable = ArpTable { atMacs        :: !(HT.HashTable IP4 Entry)
                         , atLifetime    :: !NominalDiffTime
                         , atPurgeThread :: !ThreadId
                         }

data Entry = Waiting [Maybe Mac -> IO ()]
           | Present !UTCTime !Mac


newArpTable :: Config -> IO ArpTable
newArpTable Config { .. } =
  do atMacs        <- HT.newHashTable cfgArpTableSize
     atPurgeThread <- forkNamed "Arp Purge Thread"
                          (purgeArpTable cfgArpTableLifetime atMacs)
     return ArpTable { atLifetime = cfgArpTableLifetime, .. }


-- | Loops forever, delaying until the next arp table entry needs to be purged.
-- If no entries exist, it waits for the maximum entry lifetime before checking
-- again.
purgeArpTable :: NominalDiffTime -> HT.HashTable IP4 Entry -> IO ()
purgeArpTable lifetime table = forever $
  do now <- getCurrentTime
     HT.filterHashTable (update now) table
     threadDelay delay

  where

  update _   _ Waiting{}          = False
  update now _ (Present expire _) = expire < now

  delay = toUSeconds lifetime


-- | Lookup an entry in the Arp table.
lookupEntry :: ArpTable -> IP4 -> IO (Maybe Mac)
lookupEntry ArpTable { .. } spa =
  do mb <- HT.lookup spa atMacs
     case mb of
       Just (Present _ mac) -> return (Just mac)
       _                    -> return Nothing


-- | Insert an entry into the Arp table, and unblock any waiting actions.
addEntry :: ArpTable -> IP4 -> Mac -> IO ()
addEntry ArpTable { .. } spa sha =
  do now <- getCurrentTime
     let end = addUTCTime atLifetime now

     waiters <- HT.alter (update end) spa atMacs

     -- NOTE: as we don't allow user-supplied IO actions to be registered as
     -- callbacks, we don't catch any exceptions here; the only way that an
     -- action can end up in this list is through the constructors for the
     -- WaitStrategy type defined below.
     --
     -- If it turns out that it's significantly impacting the performance of the
     -- fast path, we should consider forking a thread to run the waiters.
     mapM_ ($ Just sha) waiters

  where

  update expire (Just (Waiting ks)) = (Just (Present expire sha), ks)
  update expire (Just Present{})    = (Just (Present expire sha), [])
  update expire Nothing             = (Just (Present expire sha), [])


-- | If nobody has responded to queries for this address, notify any waiters
-- that there is no mac associated.
--
-- NOTE: in the future, it might be nice to keep an entry in the table that
-- indicates that this host is unreachable.
markUnreachable :: ArpTable -> IP4 -> IO ()
markUnreachable ArpTable { .. } addr =
  do waiters <- HT.alter update addr atMacs

     -- See the note in addEntry about why we don't need to sandbox the
     -- callbacks.
     mapM_ ($ Nothing) waiters

  where

  update (Just (Waiting ks))  = (Nothing, ks)
  update ent@(Just Present{}) = (ent, [])
  update Nothing              = (Nothing,[])



newtype WaitStrategy res = WaitStrategy { getWaiter :: IO (Maybe Mac -> IO (), res) }


-- | Have the entry block on a 
blockingStrategy :: WaitStrategy (MVar (Maybe Mac))
blockingStrategy  = WaitStrategy $
  do mvar <- newEmptyMVar

     let write mb = do _ <- tryPutMVar mvar mb
                       return ()

     return (write, mvar)


-- | Write the discovered Mac to a bounded channel, passing it through a
-- filtering function first.
writeChanStrategy :: Maybe DeviceStats -> (Maybe Mac -> Maybe msg) -> BC.BoundedChan msg
                  -> WaitStrategy ()
writeChanStrategy mbStats f chan = WaitStrategy (return (handler,()))
  where
  handler mb =
    case f mb of
      Just msg -> do written <- BC.tryWriteChan chan msg
                     case mbStats of
                       Just stats | not written -> updateError statTX stats
                       _                        -> return ()

      -- XXX should this update a stat?
      Nothing  -> return ()


data QueryResult res = Known !Mac
                     | Unknown !Bool res


-- | Returns either the address, or an empty 'MVar' that will eventually contain
-- the 'Mac', or 'Nothing' if the Arp request fails.
resolveAddr :: ArpTable -> IP4 -> WaitStrategy res -> IO (QueryResult res)
resolveAddr arp addr strategy =
  do mb <- lookupEntry arp addr
     case mb of
       Just mac -> return (Known mac)
       Nothing  -> registerWaiter arp addr strategy


-- | Register to wait on an entry in the table.
registerWaiter :: ArpTable -> IP4 -> WaitStrategy res -> IO (QueryResult res)
registerWaiter ArpTable { .. } addr strategy =
  do waiter <- getWaiter strategy
     HT.alter (update waiter) addr atMacs
  where
  update (w,r) (Just (Waiting ws))        = (Just (Waiting (w:ws)), Unknown False r)
  update _     ent@(Just (Present _ mac)) = (ent, Known mac)
  update (w,r) Nothing                    = (Just (Waiting [w]), Unknown True r)
