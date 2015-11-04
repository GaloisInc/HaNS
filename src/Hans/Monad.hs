module Hans.Monad (
    Hans()
  , runHans
  , setEscape, escape, dropPacket
  , stm
  , io
  , decode, decode'
  ) where

import Hans.Device (DeviceStats(), updateDropped)

import           Control.Monad.STM (STM,atomically)
import qualified Data.ByteString as S
import           Data.Serialize.Get (runGet,runGetState,Get)


newtype Hans a = Hans { unHans :: IO () -> (a -> IO ()) -> IO () }

instance Functor Hans where
  fmap f m = Hans (\ e k -> unHans m e (k . f))
  {-# INLINE fmap #-}

instance Applicative Hans where
  pure x  = Hans (\ _ k -> k x)

  f <*> x = Hans $ \ e k -> unHans f e
                 $ \ g   -> unHans x e
                 $ \ y   -> k (g y)

  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Monad Hans where
  return  = pure

  m >>= f = Hans $ \ e k -> unHans m e
                 $ \ a   -> unHans (f a) e k

  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}


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

-- | Synonym for 'escape'.
dropPacket :: DeviceStats -> Hans a
dropPacket stats =
  do stm (updateDropped stats)
     escape
{-# INLINE dropPacket #-}

-- | Lift an 'STM' operation into 'Hans'.
stm :: STM a -> Hans a
stm m = io (atomically m)
{-# INLINE stm #-}

-- | Lift an 'IO' operation into 'Hans'.
io :: IO a -> Hans a
io m = Hans (\ _ k -> m >>= k )
{-# INLINE io #-}

-- | Run a Get action in the context of the Hans monad, 
decode :: DeviceStats -> Get a -> S.ByteString -> Hans a
decode stats m bytes =
  case runGet m bytes of
    Right a -> return a

    -- XXX what should we do with the error text?
    Left _err -> dropPacket stats
{-# INLINE decode #-}

-- | Run a Get action in the context of the Hans monad, returning any unconsumed
-- input.
decode' :: DeviceStats -> Get a -> S.ByteString -> Hans (a,S.ByteString)
decode' stats m bytes =
  case runGetState m bytes 0 of
    Right a -> return a

    -- XXX what should we do with the error text?
    Left _err -> dropPacket stats
