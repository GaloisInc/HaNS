{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE Rank2Types             #-}

module Hans.Layer where

import Hans.Utils (just)

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad (ap,MonadPlus(mzero,mplus))
import Data.Monoid (Monoid(..))
import Data.Time.Clock.POSIX
import MonadLib (StateM(get,set))
import qualified Control.Exception as X
import qualified Data.Map as Map

data LayerState i = LayerState
  { lsNow     :: POSIXTime
  , lsState   :: i
  }

data Action = Nop | Action (IO ())

instance Monoid Action where
  mempty = Nop

  mappend (Action a) (Action b) = Action (a >> b)
  mappend Nop        b          = b
  mappend a          _          = a

runAction :: Action -> IO ()
runAction Nop        = return ()
runAction (Action m) = m `X.catch` \ se -> print (se :: X.SomeException)

data Result i a
  = Error Action
  | Result (LayerState i) a Action

-- | Failure continuation
type Failure i r = Action -> Result i r

-- | Success continuation
type Success a i r = a -> LayerState i -> Action -> Result i r

newtype Layer i a = Layer
  { getLayer :: forall r. LayerState i -> Action
                       -> Failure i r -> Success a i r
                       -> Result i r }

runLayer :: LayerState i -> Layer i a -> Result i a
runLayer i0 m = getLayer m i0 mempty Error success
  where success a i o = Result i a o

loopLayer :: i -> IO msg -> (msg -> Layer i ()) -> IO ()
loopLayer i0 msg k = loop (LayerState 0 i0)
  where
  loop i = do
    a   <- msg
    now <- getPOSIXTime
    let res = runLayer (i {lsNow = now }) (k a)
    X.evaluate res `X.catch` \ se -> print (se :: X.SomeException) >> return res
    case res of
      Error        m -> runAction m >> loop i
      Result i' () m -> runAction m >> loop i'

instance Functor (Layer i) where
  fmap g m = Layer (\i0 o0 f k -> getLayer m i0 o0 f (\a i o -> k (g a) i o))

instance Applicative (Layer i) where
  pure  = return
  (<*>) = ap

instance Alternative (Layer i) where
  empty   = Layer (\_ o0 f _ -> f o0)
  a <|> b = Layer (\i o f k -> getLayer a i o (\_ -> getLayer b i o f k) k)

instance Monad (Layer i) where
  return x = Layer (\i o _ k -> k x i o)
  m >>= g = Layer $ \i0 o0 f k -> getLayer m i0 o0 f $ \a i o ->
                                  getLayer (g a) i o f k

instance MonadPlus (Layer i) where
  mzero = empty
  mplus = (<|>)

instance StateM (Layer i) i where
  get   = Layer (\i0 o0 _ k -> k (lsState i0) i0 o0)
  set i = Layer (\i0 o0 _ k -> k () (i0 { lsState = i }) o0)


-- Utilities -------------------------------------------------------------------

dropPacket :: Layer i a
dropPacket  = empty

time :: Layer i POSIXTime
time  = Layer $ \i0 o0 _ k -> k (lsNow i0) i0 o0

output :: IO () -> Layer i ()
output m = Layer $ \i0 o0 _ k -> k () i0 (o0 `mappend` Action m)

liftRight :: Either String b -> Layer i b
liftRight (Right b)  = return b
liftRight (Left err) = do
  output (putStrLn err)
  dropPacket

-- Handler Generalization ------------------------------------------------------

type Handlers k a = Map.Map k a

emptyHandlers :: Handlers k a
emptyHandlers  = Map.empty


class ProvidesHandlers i k a | i -> k a where
  getHandlers :: i -> Handlers k a
  setHandlers :: Handlers k a -> i -> i


getHandler :: (Ord k, ProvidesHandlers i k a) => k -> Layer i a
getHandler k = do
  state <- get
  just (Map.lookup k (getHandlers state))


addHandler :: (Ord k, ProvidesHandlers i k a) => k -> a -> Layer i ()
addHandler k a = do
  state <- get
  let hs' = Map.insert k a (getHandlers state)
  hs' `seq` set (setHandlers hs' state)


removeHandler :: (Ord k, ProvidesHandlers i k a) => k -> Layer i ()
removeHandler k = do
  state <- get
  let hs' = Map.delete k (getHandlers state)
  hs' `seq` set (setHandlers hs' state)
