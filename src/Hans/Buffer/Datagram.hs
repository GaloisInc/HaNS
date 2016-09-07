{-# LANGUAGE RecordWildCards #-}

module Hans.Buffer.Datagram (
    Buffer,
    newBuffer,
    writeChunk,
    readChunk,
    tryReadChunk,
    isEmptyBuffer
  ) where

import Hans.Buffer.Signal

import           Control.Monad (when)
import qualified Data.ByteString as S
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import qualified Data.Sequence as Seq


-- Buffers ---------------------------------------------------------------------

data Buffer a = Buffer { bufContents :: {-# UNPACK #-} !(IORef (BufContents a))
                       , bufSignal   :: {-# UNPACK #-} !Signal
                         -- ^ The wait queue. Waiters queue up trying to take the
                         -- MVar, and are unblocked when there are chunks
                         -- available in the queue.
                       }

newBuffer :: Int -> IO (Buffer a)
newBuffer size =
  do bufContents <- newIORef (emptyBufContents size)
     bufSignal   <- newSignal
     return Buffer { .. }

-- | Write a chunk to the buffer. Returns 'False' if the chunk could not be
-- written.
writeChunk :: Buffer a -> a -> S.ByteString -> IO Bool
writeChunk Buffer { .. } a bytes =
  do (written,more) <- atomicModifyIORef' bufContents (queueChunk a bytes)
     when more (signal bufSignal)
     return written


-- | Read a chunk from the buffer, blocking until one is ready. 
readChunk :: Buffer a -> IO (a,S.ByteString)
readChunk Buffer { .. } = loop
  where
  loop =
    do mb <- atomicModifyIORef' bufContents dequeueChunk
       case mb of
         Just (c,more) ->
           do when more (signal bufSignal)
              return c

         Nothing ->
           do waitSignal bufSignal
              loop


-- | Poll for a ready chunk.
tryReadChunk :: Buffer a -> IO (Maybe (a,S.ByteString))
tryReadChunk Buffer { .. } =
  do mb <- atomicModifyIORef' bufContents dequeueChunk
     case mb of
       Just (a,more) ->
         do when more (signal bufSignal)
            return (Just a)

       Nothing ->
            return Nothing

-- | See if the buffer is empty.
isEmptyBuffer :: Buffer a -> IO Bool
isEmptyBuffer Buffer { .. } =
  (Seq.null . bufChunks) `fmap` readIORef bufContents

-- Buffer State ----------------------------------------------------------------

data BufContents a = BufContents { bufAvail :: {-# UNPACK #-} !Int
                                   -- ^ Available space in the buffer, in bytes.

                                 , bufChunks  :: !(Seq.Seq (a,S.ByteString))
                                   -- ^ Chunks present in the buffer.
                                 }


emptyBufContents :: Int -> BufContents a
emptyBufContents bufAvail = BufContents { bufChunks = Seq.empty, .. }

chunksAvailable :: BufContents a -> Bool
chunksAvailable BufContents { .. } = not (Seq.null bufChunks)

-- | The return value is as follows:
--
--  The first element is 'True' when the chunk has been written to the queue
--  The second element is 'True' when there is more data present in the queue
--
-- This covers a funny case where the queue was empty, and the chunk that was
-- given was too big for the whole buffer -- the buffer wasn't written, and the
-- queue is still empty.
queueChunk :: a -> S.ByteString -> BufContents a -> (BufContents a,(Bool,Bool))
queueChunk a chunk buf
  | bufAvail buf >= chunkLen =
    (BufContents { bufAvail  = bufAvail buf - chunkLen
                 , bufChunks = bufChunks buf Seq.|> (a,chunk) }, (True,True))

  | otherwise =
    (buf, (False,chunksAvailable buf))

  where
  chunkLen = S.length chunk

dequeueChunk :: BufContents a -> (BufContents a, Maybe ((a,S.ByteString),Bool))
dequeueChunk buf =
  case Seq.viewl (bufChunks buf) of
    c Seq.:< cs ->
      let buf' = BufContents { bufAvail  = bufAvail buf + S.length (snd c)
                             , bufChunks = cs }
       in (buf', Just (c, chunksAvailable buf'))

    _ -> (buf, Nothing)
