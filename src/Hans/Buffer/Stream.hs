{-# LANGUAGE RecordWildCards #-}

-- | The rationale behind this buffer not tracking any size parameter, is that
-- it will always be used as part of the receive window. As such, there should
-- only ever be at most a full window's worth of data queued, as other data will
-- be rejected.

module Hans.Buffer.Stream (
    Buffer(),
    newBuffer,
    closeBuffer,
    bytesAvailable,
    putBytes,
    takeBytes,
    tryTakeBytes,
  ) where

import Hans.Buffer.Signal

import           Control.Monad (when,unless)
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
  do bufState  <- newIORef emptyState
     bufSignal <- newSignal
     return Buffer { .. }

closeBuffer :: Buffer -> IO ()
closeBuffer Buffer { .. } =
  do atomicModifyIORef' bufState sClose
     signal bufSignal

bytesAvailable :: Buffer -> IO Bool
bytesAvailable Buffer { .. } =
  do st <- readIORef bufState
     return (not (sNull st))

putBytes :: S.ByteString -> Buffer -> IO ()
putBytes bytes Buffer { .. } =
  do unless (S.null bytes) (atomicModifyIORef' bufState (sPut bytes))
     signal bufSignal

-- | Take up to n bytes from the buffer, blocking until some data is ready.
takeBytes :: Int -> Buffer -> IO L.ByteString
takeBytes n Buffer { .. }
  | n <= 0    = return L.empty
  | otherwise = loop
  where
  loop =
    do (bytes,more,closed) <- atomicModifyIORef' bufState (sTake n)
       if L.null bytes && not closed
          then do waitSignal bufSignal
                  loop
          else do when (more || closed) (signal bufSignal)
                  return bytes

-- | Take up to n bytes from the buffer, returning immediately if no data is
-- available.
tryTakeBytes :: Int -> Buffer -> IO (Maybe L.ByteString)
tryTakeBytes n Buffer { .. }
  | n <= 0    = return Nothing
  | otherwise =
    do (bytes,more,closed) <- atomicModifyIORef' bufState (sTake n)
       when (more || closed) (signal bufSignal)
       if L.null bytes
          then return Nothing
          else return (Just bytes)


-- Internal State --------------------------------------------------------------

data State = State { stBuf    :: !(Seq.Seq S.ByteString)
                   , stClosed :: !Bool
                   }

emptyState :: State
emptyState  = State { stBuf = Seq.empty, stClosed = False }

sNull :: State -> Bool
sNull State { .. } = Seq.null stBuf

sClose :: State -> (State, ())
sClose State { .. } = (State { stClosed = True, .. }, ())

-- | Remove up to n bytes of data from the internal state.
sTake :: Int -> State -> (State, (L.ByteString,Bool,Bool))
sTake n0 State { .. } = go [] n0 stBuf
  where

  go acc n mem
    | n > 0 =
      case Seq.viewl mem of

        buf Seq.:< mem'

          | S.length buf > n ->
            let (as,bs) = S.splitAt n buf
             in finalize (L.fromStrict as:acc) (bs Seq.<| mem')

          | otherwise ->
            go (L.fromStrict buf:acc) (n - S.length buf) mem'


        Seq.EmptyL ->
          finalize acc Seq.empty

    | otherwise =
      finalize acc mem

  finalize acc mem =
    ( State { stBuf = mem, .. }
    , (L.concat (reverse acc), not (Seq.null mem), stClosed)
    )


sPut :: S.ByteString -> State -> (State, ())
sPut bytes State { .. } = (State { stBuf = stBuf Seq.|> bytes, .. }, ())
