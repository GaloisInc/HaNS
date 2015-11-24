{-# LANGUAGE RecordWildCards #-}

module Hans.Buffer.Datagram (
    Buffer,
    newBuffer,
    writeChunk,
    readChunk,
    tryReadChunk,
  ) where

import           Control.Concurrent
                     (MVar,newEmptyMVar,tryPutMVar,takeMVar,tryTakeMVar)
import           Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import qualified Data.Sequence as Seq


-- Buffers ---------------------------------------------------------------------

type Signal = MVar ()

data Buffer a = Buffer { bufContents :: {-# UNPACK #-} !(IORef (BufContents a))
                       , bufNotify   :: {-# UNPACK #-} !Signal
                         -- ^ The wait queue. Waiters queue up trying to take the
                         -- MVar, and are unblocked when there are chunks
                         -- available in the queue.
                       }

newBuffer :: Int -> IO (Buffer a)
newBuffer size =
  do bufContents <- newIORef (emptyBufContents size)
     bufNotify   <- newEmptyMVar
     return Buffer { .. }

signal :: Signal -> IO ()
signal mvar =
  do _ <- tryPutMVar mvar ()
     return ()

-- | Write a chunk to the buffer. Returns 'False' if the chunk could not be
-- written.
writeChunk :: Buffer a -> a -> S.ByteString -> IO Bool
writeChunk Buffer { .. } a bytes =
  do (written,more) <- atomicModifyIORef' bufContents (queueChunk a bytes)
     when more (signal bufNotify)
     return written


-- | Read a chunk from the buffer, blocking until one is ready. 
readChunk :: Buffer a -> IO (a,S.ByteString)
readChunk Buffer { .. } =
  do ()           <- takeMVar bufNotify
     (bytes,more) <- atomicModifyIORef' bufContents dequeueChunk
     when more (signal bufNotify)
     return bytes


-- | Poll for a ready chunk.
tryReadChunk :: Buffer a -> IO (Maybe (a,S.ByteString))
tryReadChunk Buffer { .. } =
  do mb <- tryTakeMVar bufNotify
     case mb of

       Just () ->
         do (bytes,more) <- atomicModifyIORef' bufContents dequeueChunk
            when more (signal bufNotify)
            return (Just bytes)

       Nothing -> return Nothing


-- Buffer State ----------------------------------------------------------------

data BufContents a = BufContents { bufAvail :: {-# UNPACK #-} !Int
                                   -- ^ Available space in the buffer, in bytes.

                                 , bufChunks  :: !(Seq.Seq (a,S.ByteString))
                                   -- ^ Chunks present in the buffer.
                                 }


emptyBufContents :: Int -> BufContents a
emptyBufContents bufAvail = BufContents { bufChunks = Seq.empty, .. }

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
    (buf, (False,Seq.null (bufChunks buf)))

  where
  chunkLen = S.length chunk

dequeueChunk :: BufContents a -> (BufContents a, ((a,S.ByteString),Bool))
dequeueChunk buf =
  case Seq.viewl (bufChunks buf) of
    c Seq.:< cs -> (BufContents { bufAvail  = bufAvail buf - S.length (snd c)
                                , bufChunks = cs }, (c, Seq.null cs))

    _ -> error $ unlines
         [ "PANIC: Hans.Buffer.Datagram.dequeueChunk:"
         , "  Invariant violated: dequeueChunk called on empty buffer"
         ]
