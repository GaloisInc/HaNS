{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Hans.IP4.Fragments (
    FragTable(),
    newFragTable,
    processFragment,
  ) where

import           Hans.Config
import qualified Hans.HashTable as HT
import           Hans.IP4.Packet
import           Hans.Monad
import           Hans.Time (toUSeconds)

import           Control.Concurrent (forkIO,ThreadId,threadDelay)
import           Control.Monad (forever)
import qualified Data.ByteString as S
import           Data.Time.Clock
                     (UTCTime,getCurrentTime,NominalDiffTime,addUTCTime)


-- | Keys are of the form @(src,dest,prot,ident)@.
type Key = (IP4,IP4,IP4Protocol,IP4Ident)

type Table  = HT.HashTable Key Buffer

-- XXX: there isn't any way to limit the size of the fragment table right now.
data FragTable = FragTable { ftEntries     :: !Table
                           , ftDuration    :: !NominalDiffTime
                           , ftPurgeThread :: !ThreadId
                           }

newFragTable :: Config -> IO FragTable
newFragTable Config { .. } =
  do ftEntries     <- HT.newHashTable 31 -- XXX: is this the best table size?
     ftPurgeThread <- forkIO (purgeEntries cfgIP4FragTimeout ftEntries)
     return FragTable { ftDuration = cfgIP4FragTimeout, .. }


-- | Handle an incoming fragment. If the fragment is buffered, but doesn't
-- complete the packet, the escape continuation is invoked.
processFragment :: FragTable -> IP4Header -> S.ByteString
                -> Hans (IP4Header,S.ByteString)

processFragment FragTable { .. } hdr body

    -- no fragments
  | not (ip4MoreFragments hdr) && ip4FragmentOffset hdr == 0 =
    return (hdr,body)

    -- fragment
  | otherwise =
    do mb <- io $ do now <- getCurrentTime
                     let expire = addUTCTime ftDuration now
                         frag   = mkFragment hdr body
                         key    = mkKey hdr

                     HT.alter (updateBuffer expire hdr frag) key ftEntries
       case mb of
         -- abort packet processing here, as there's nothing left to do
         Nothing           -> escape

         -- return the reassembled packet
         Just (hdr',body') -> return (hdr',body')
{-# INLINE processFragment #-}


-- Table Purging ---------------------------------------------------------------

-- | Every second, purge the fragment table of entries that have expired.
purgeEntries :: NominalDiffTime -> Table -> IO ()
purgeEntries lifetime entries = forever $
  do threadDelay halfLife

     now <- getCurrentTime
     HT.filterHashTable (\_ Buffer { .. } -> bufExpire < now) entries

  where
  halfLife = toUSeconds (lifetime / 2)


-- Fragment Operations ---------------------------------------------------------

-- INVARIANT: When new fragments are inserted into bufFragments, they are merged
-- together when possible. This makes it easier to check the state of the whole
-- buffer.
data Buffer = Buffer { bufExpire    :: !UTCTime
                     , bufHeader    :: !(Maybe IP4Header)
                     , bufFragments :: ![Fragment]
                     }

data Fragment = Fragment { fragStart   :: {-# UNPACK #-} !Int
                         , fragEnd     :: {-# UNPACK #-} !Int
                         , fragPayload ::                 [S.ByteString]
                         }

mkKey :: IP4Header -> Key
mkKey IP4Header { .. } = (ip4SourceAddr,ip4DestAddr,ip4Protocol,ip4Ident)


mkFragment :: IP4Header -> S.ByteString -> Fragment
mkFragment hdr body = Fragment { .. }
  where
  fragStart   = fromIntegral (ip4FragmentOffset hdr)
  fragEnd     = fragStart + S.length body
  fragPayload = [body]


-- | Create a buffer, given an expiration time, initial fragment, and
-- 'IP4Header' of that initial fragment. The initial header is included for the
-- case where the initial fragment is also the first fragment in the sequence.
mkBuffer :: UTCTime -> IP4Header -> Fragment -> Buffer
mkBuffer bufExpire hdr frag =
  addFragment hdr frag
  Buffer { bufHeader    = Nothing
         , bufFragments = []
         , .. }


-- | For use with HT.alter. When the first element is 'Just', the second will
-- be 'Nothing', indicating that the entry in the table should be updated, and
-- there's no result yet. When the first element is 'Nothing', the second will
-- be 'Just', indicating that the entry should be removed from the table, and
-- that this is the final buffer.
updateBuffer :: UTCTime -> IP4Header -> Fragment -> Maybe Buffer
               -> (Maybe Buffer,Maybe (IP4Header,S.ByteString))

-- the entry already exists in the table, removing it if it's full
updateBuffer _ hdr frag (Just buf) =
  let buf' = addFragment hdr frag buf
   in case bufFull buf' of
        Just res -> (Nothing, Just res)
        Nothing  -> (Just buf', Nothing)

-- create a new entry in the table
updateBuffer expire hdr frag Nothing =
  let buf = mkBuffer expire hdr frag
   in buf `seq` (Just buf, Nothing)


-- | When the buffer is full and all fragments are accounted for, reassemble it
-- into a new packet.
bufFull :: Buffer -> Maybe (IP4Header,S.ByteString)
bufFull Buffer { .. }

  | Just hdr <- bufHeader
  , [Fragment { .. }] <- bufFragments =
    Just (hdr, S.concat fragPayload)

  | otherwise =
    Nothing


-- | Insert the fragment into the buffer.
addFragment :: IP4Header -> Fragment -> Buffer -> Buffer
addFragment hdr frag buf =
  Buffer { bufExpire    = bufExpire buf
         , bufHeader    = case bufHeader buf of
                            Nothing | ip4FragmentOffset hdr == 0 -> Just hdr
                            _                                    -> bufHeader buf

         , bufFragments = insertFragment (bufFragments buf)
         }

  where

  insertFragment frags@(f:fs)
    | fragEnd   frag + 1 == fragStart f = mergeFragment frag f : fs
    | fragStart frag + 1 == fragEnd f   = mergeFragment f frag : fs
    | fragStart frag     <  fragStart f = frag : frags
    | otherwise                         = f : insertFragment fs

  insertFragment [] = [frag]


mergeFragment :: Fragment -> Fragment -> Fragment
mergeFragment a b =
  Fragment { fragStart   = fragStart a
           , fragEnd     = fragEnd b
           , fragPayload = fragPayload a ++ fragPayload b
           }
