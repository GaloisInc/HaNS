{-# LANGUAGE RecordWildCards #-}

-- | The rationale behind this buffer not tracking any size parameter, is that
-- it will always be used as part of the receive window. As such, there should
-- only ever be at most a full window's worth of data queued, as other data will
-- be rejected.

module Hans.Buffer.Stream (
    Buffer(),
    newBuffer,
    bytesAvailable,
    putBytes,
    takeBytes,
    tryTakeBytes,
  ) where

import Hans.Buffer.Signal

import           Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import qualified Data.Sequence as Seq


-- Stream Buffers --------------------------------------------------------------

data Buffer = Buffer { bufState :: !(IORef State)
                       -- ^ The buffer and current size

                     , bufSignal :: !Signal
                       -- ^ Data available signal
                     }

newBuffer :: IO Buffer
newBuffer  =
  do bufState  <- newIORef Seq.empty
     bufSignal <- newSignal
     return Buffer { .. }

bytesAvailable :: Buffer -> IO Bool
bytesAvailable Buffer { .. } =
  do st <- readIORef bufState
     return (not (Seq.null st))

putBytes :: S.ByteString -> Buffer -> IO ()
putBytes bytes Buffer { .. }
  | S.null bytes = signal bufSignal
  | otherwise    = do atomicModifyIORef' bufState (sPut bytes)
                      signal bufSignal

-- | Take up to n bytes from the buffer, blocking until some data is ready.
takeBytes :: Int -> Buffer -> IO L.ByteString
takeBytes n Buffer { .. } =
  do waitSignal bufSignal
     (bytes,more) <- atomicModifyIORef' bufState (sTake n)
     when more (signal bufSignal)
     return bytes

-- | Take up to n bytes from the buffer, returning immediately if no data is
-- available.
tryTakeBytes :: Int -> Buffer -> IO (Maybe L.ByteString)
tryTakeBytes n Buffer { .. } =
  do available <- tryWaitSignal bufSignal
     if available
        then do (bytes,more) <- atomicModifyIORef' bufState (sTake n)
                when more (signal bufSignal)
                return (Just bytes)

        else return Nothing


-- Internal State --------------------------------------------------------------

type State = Seq.Seq S.ByteString

-- | Remove up to n bytes of data from the internal state.
sTake :: Int -> State -> (State, (L.ByteString,Bool))
sTake n0 = go L.empty n0
  where
  go acc n mem = case Seq.viewl mem of

    buf Seq.:< mem' ->
      case compare (S.length buf) n of
        LT -> go (L.append acc (L.fromStrict buf)) (n - S.length buf) mem'
        EQ -> finalize acc mem'
        GT -> let (as,bs) = S.splitAt n buf
               in finalize (L.append acc (L.fromStrict as)) (bs Seq.<| mem')

    Seq.EmptyL ->
      finalize acc Seq.empty

  finalize acc mem = (mem,(acc, not (Seq.null mem)))


sPut :: S.ByteString -> State -> (State, ())
sPut bytes buf = (buf Seq.|> bytes, ())
