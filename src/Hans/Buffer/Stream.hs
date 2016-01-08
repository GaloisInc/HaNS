{-# LANGUAGE RecordWildCards #-}

module Hans.Buffer.Stream where

import           Control.Concurrent
                     (MVar,newEmptyMVar,tryPutMVar,takeMVar,tryTakeMVar)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef')
import           Data.Int (Int64)
import qualified Data.Sequence as Seq


-- Stream Buffers --------------------------------------------------------------

data Buffer = Buffer { bufSize :: !Int64
                       -- ^ The maximum size of the buffer contents

                     , bufState :: !(IORef State)
                       -- ^ The buffer and current size

                     , bufSignal :: !(MVar ())
                       -- ^ Data available signal
                     }

newBuffer :: Int64 -> IO Buffer
newBuffer bufSize =
  do bufState  <- newIORef (emptyState (fromIntegral bufSize))
     bufSignal <- newEmptyMVar
     return Buffer { .. }

-- | Take up to n bytes from the buffer, blocking until some data is ready.
takeBytes :: Int64 -> Buffer -> IO L.ByteString
takeBytes n Buffer { .. } =
  do ()           <- takeMVar bufSignal
     (bytes,more) <- atomicModifyIORef' bufState (sTake n)
     when more $ do _ <- tryPutMVar bufSignal ()
                    return ()
     return bytes

-- | Take up to n bytes from the buffer, returning immediately if no data is
-- available.
tryTakeBytes :: Int64 -> Buffer -> IO (Maybe L.ByteString)
tryTakeBytes n Buffer { .. } =
  do mb <- tryTakeMVar bufSignal
     case mb of
       Just () ->
         do (bytes,more) <- atomicModifyIORef' bufState (sTake n)
            when more $ do _ <- tryPutMVar bufSignal ()
                           return ()
            return (Just bytes)

       Nothing ->
         return Nothing


-- Internal State --------------------------------------------------------------

data State = State { sMem   :: !(Seq.Seq L.ByteString)
                   , sAvail :: !Int64
                   }

emptyState :: Int64 -> State
emptyState sAvail = State { sMem = Seq.empty, .. }

-- | Remove up to n bytes of data from the internal state.
sTake :: Int64 -> State -> (State, (L.ByteString,Bool))
sTake n0 State { .. } = go L.empty n0 sMem
  where
  go acc n mem = case Seq.viewl mem of

    buf Seq.:< mem' ->
      case compare (L.length buf) n of
        LT -> go (L.append acc buf) (n - L.length buf) mem'
        EQ -> finalize acc mem'
        GT -> let (as,bs) = L.splitAt n buf
               in finalize (L.append acc as) (bs Seq.<| mem')

    Seq.EmptyL ->
      finalize acc Seq.empty

  finalize acc mem = ( State { sAvail = sAvail + L.length acc, sMem = mem }
                     , (acc, not (Seq.null mem)) )
