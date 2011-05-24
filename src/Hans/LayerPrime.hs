{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module Hans.LayerPrime where

import Hans.Device (Add(..))
import Hans.Message (Message(sendTo))

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Data.Time.Clock.POSIX
import MonadLib
import qualified Data.Map as Map


-- Continuation Monad ----------------------------------------------------------

newtype Cont m r a = Cont { getCont :: (a -> m r) -> m r }

{-# INLINE runCont #-}
runCont :: (a -> m r) -> Cont m r a -> m r
runCont k m = getCont m k

instance Functor (Cont m r) where
  fmap f m = Cont (\k -> runCont (k . f) m)

instance Applicative (Cont m r) where
  pure a  = Cont (\k -> k a)
  f <*> x = Cont (\k -> getCont f (\g -> getCont x (\y -> k (g y))))

instance Monad (Cont m r) where
  {-# INLINE return #-}
  return a = pure a
  m >>= f  = Cont (\k -> runCont (runCont k . f) m)

instance StateM m i => StateM (Cont m r) i where
  get   = liftC get
  set i = liftC (set i)

instance MonadPlus m => MonadPlus (Cont m r) where
  mzero     = liftC mzero
  mplus a b = Cont (\k -> getCont a k `mplus` getCont b k)

liftC :: Monad m => m a -> Cont m r a
liftC m = Cont (=<< m)


-- Network Stack Layer Monad ---------------------------------------------------

data LayerState i = LayerState
  { lsNow     :: POSIXTime
  , lsState   :: i
  }

data Action = Nop | Action (IO ())

instance Monoid Action where
  {-# INLINE mempty #-}
  mempty = Nop

  mappend (Action a) (Action b) = Action (a >> b)
  mappend Nop        b          = b
  mappend a          _          = a

{-# INLINE runAction #-}
runAction :: Action -> IO ()
runAction Nop        = return ()
runAction (Action m) = m

type Layer i = Cont (LayerM i) ()

data Result i a
  = Error Action
  | Result (LayerState i) a Action

newtype LayerM i a = LayerM { getLayerM :: LayerState i -> Result i a }

{-# SPECIALIZE runLayerM :: LayerState i -> LayerM i () -> Result i () #-}
runLayerM :: LayerState i -> LayerM i a -> Result i a
runLayerM i m = getLayerM m i

{-# INLINE loopLayer #-}
loopLayer :: i -> IO msg -> (msg -> Layer i ()) -> IO ()
loopLayer i0 msg k = loop (LayerState 0 i0)
  where
  loop i = do
    a   <- msg
    now <- getPOSIXTime
    case runLayerM (i {lsNow = now }) (runCont return (k a)) of
      Error        a -> runAction a >> loop i
      Result i' () a -> runAction a >> loop i'

instance Functor (LayerM i) where
  fmap f m = LayerM $ \i0 ->
    case runLayerM i0 m of
      Error k      -> Error k
      Result i a k -> Result i (f a) k

instance Applicative (LayerM i) where
  pure x = LayerM $ \i0 -> Result i0 x mempty

  f <*> x = LayerM $ \i0 -> case runLayerM i0 f of
    Error k      -> Error k
    Result i g k -> case runLayerM i x of
      Error l      -> Error (k `mappend` l)
      Result i y l -> Result i (g y) (k `mappend` l)

instance Alternative (LayerM i) where
  {-# INLINE empty #-}
  empty = LayerM $ \ _ -> Error mempty

  {-# INLINE (<|>) #-}
  a <|> b = LayerM $ \i0 -> case runLayerM i0 a of
    Error _      -> runLayerM i0 b
    Result i a k -> Result i a k

instance Monad (LayerM i) where
  {-# INLINE return #-}
  return x = pure x

  m >>= f = LayerM $ \i0 -> case runLayerM i0 m of
    Error k      -> Error k
    Result i a k -> case runLayerM i (f a) of
      Error l      -> Error (k `mappend` l)
      Result i b l -> Result i b (k `mappend` l)

instance MonadPlus (LayerM i) where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = (<|>)

instance StateM (LayerM i) i where
  get   = LayerM $ \i0 -> Result i0 (lsState i0) mempty
  set i = LayerM $ \i0 -> Result (i0 { lsState = i }) () mempty


-- Utilities -------------------------------------------------------------------

{-# INLINE dropPacket #-}
dropPacket :: Layer i a
dropPacket  = liftC empty

time :: Layer i POSIXTime
time  = liftC $ LayerM $ \i0 -> Result i0 (lsNow i0) mempty

output :: IO () -> Layer i ()
output m = liftC $ LayerM $ \i0 -> Result i0 () (Action m)

-- Handler Generalization ------------------------------------------------------

type Handlers k a = Map.Map k a

emptyHandlers :: Handlers k a
emptyHandlers  = Map.empty


class ProvidesHandlers i k a | i -> k a where
  getHandlers :: i -> Handlers k a
  setHandlers :: Handlers k a -> i -> i


setHandler :: Message h (Add (k,a)) => h -> k -> a -> IO ()
setHandler h k a = sendTo h (Add (k,a))


getHandler :: (Ord k, ProvidesHandlers i k a) => k -> Layer i a
getHandler k = liftC $ do
  state <- get
  case Map.lookup k (getHandlers state) of
    Nothing -> mzero
    Just h  -> return h


addHandler :: (Ord k, ProvidesHandlers i k a) => k -> a -> Layer i ()
addHandler k a = liftC $ do
  state <- get
  let hs' = Map.insert k a (getHandlers state)
  hs' `seq` set (setHandlers hs' state)


removeHandler :: (Ord k, ProvidesHandlers i k a) => k -> Layer i ()
removeHandler k = liftC $ do
  state <- get
  let hs' = Map.delete k (getHandlers state)
  hs' `seq` set (setHandlers hs' state)
