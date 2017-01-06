{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Monad (
    Hans()
  , runHans, runHansOnce
  , setEscape, escape, dropPacket
  , callCC
  , io
  , decode, decode'
  ) where

import Hans.Device.Types (DeviceStats(),updateDropped,statRX)

import qualified Control.Applicative as A
import qualified Data.ByteString as S
import           Data.IORef (newIORef,writeIORef,readIORef)
import           Data.Serialize.Get (runGet,runGetState,Get)
import           MonadLib (BaseM(..))


newtype Hans a = Hans { unHans :: IO () -> (a -> IO ()) -> IO () }

instance Functor Hans where
  fmap f m = Hans (\ e k -> unHans m e (k . f))
  {-# INLINE fmap #-}

instance A.Applicative Hans where
  pure x  = Hans (\ _ k -> k x)

  f <*> x = Hans $ \ e k -> unHans f e
                 $ \ g   -> unHans x e
                 $ \ y   -> k (g y)

  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Monad Hans where
  return  = A.pure

  m >>= f = Hans $ \ e k -> unHans m e
                 $ \ a   -> unHans (f a) e k

  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}

instance BaseM Hans IO where
  inBase = io
  {-# INLINE inBase #-}


-- | Run only one iteration of the Hans monad.
runHansOnce :: Hans a -> IO (Maybe a)
runHansOnce (Hans f) =
  do res <- newIORef Nothing
     f (writeIORef res Nothing) (\x -> writeIORef res (Just x))
     readIORef res


-- | Loop forever, running the underlying operation.
runHans :: Hans () -> IO ()
runHans (Hans m) = loop ()
  where
  loop () = m (loop ()) loop
{-# INLINE runHans #-}

-- | Set the escape continuation to the current position, within the operation
-- given, so that control flow always restores back through this point.
setEscape :: Hans () -> Hans ()
setEscape m = Hans (\ _ k -> unHans m (k ()) k)
{-# INLINE setEscape #-}

-- | Invoke the escape continuation.
escape :: Hans a
escape  = Hans (\ e _ -> e)
{-# INLINE escape #-}

-- | Call-cc. NOTE: the continuation will inherit the escape point of the
-- calling context.
callCC :: ((b -> Hans a) -> Hans b) -> Hans b
callCC f = Hans (\e k -> unHans (f (\b -> Hans (\_ _ -> k b))) e k)
{-# INLINE callCC #-}

-- | Synonym for 'escape' that also updates device statistics.
dropPacket :: DeviceStats -> Hans a
dropPacket stats =
  do io (updateDropped statRX stats)
     escape
{-# INLINE dropPacket #-}

-- | Lift an 'IO' operation into 'Hans'.
io :: IO a -> Hans a
io m = Hans (\ _ k -> m >>= k )
{-# INLINE io #-}

-- | Run a Get action in the context of the Hans monad, 
decode :: DeviceStats -> Get a -> S.ByteString -> Hans a
decode dev m bytes =
  case runGet m bytes of
    Right a -> return a
    Left _  -> dropPacket dev
{-# INLINE decode #-}

-- | Run a Get action in the context of the Hans monad, returning any unconsumed
-- input.
decode' :: DeviceStats -> Get a -> S.ByteString -> Hans (a,S.ByteString)
decode' dev m bytes =
  case runGetState m bytes 0 of
    Right a -> return a
    Left _  -> dropPacket dev
