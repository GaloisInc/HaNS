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
import MonadLib (StateM(get,set),BaseM(inBase))
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
  | Exit (LayerState i) Action
  | Result (LayerState i) a Action

-- | Early exit continuation
type Exit i r = LayerState i -> Action -> Result i r

-- | Failure continuation
type Failure i r = Action -> Result i r

-- | Success continuation
type Success a i r = a -> LayerState i -> Action -> Result i r

newtype Layer i a = Layer
  { getLayer :: forall r. LayerState i
                       -> Action
                       -> Exit i r
                       -> Failure i r
                       -> Success a i r
                       -> Result i r }

runLayer :: LayerState i -> Layer i a -> Result i a
runLayer i0 m = getLayer m i0 mempty Exit Error success
  where success a i o = Result i a o

loopLayer :: String -> i -> IO msg -> (msg -> Layer i ()) -> IO ()
loopLayer name i0 msg k =
  loop (LayerState 0 i0) `X.finally` putStrLn (name ++ " died")
  where
  loop i = do
    a   <- msg
    now <- getPOSIXTime
    let res = runLayer (i {lsNow = now }) (k a)
    _ <- X.evaluate res `X.catch` \ se -> do
           putStrLn (name ++ show (se :: X.SomeException))
           return res
    case res of
      Error        m -> runAction m >> loop i
      Exit i' m      -> runAction m >> loop i'
      Result i' () m -> runAction m >> loop i'

instance Functor (Layer i) where
  fmap g m = Layer $ \i0 o0 x f k ->
                     getLayer m i0 o0 x f (\a i o -> k (g a) i o)

instance Applicative (Layer i) where
  pure  = return
  (<*>) = ap

instance Alternative (Layer i) where
  empty   = Layer (\_ o0 _ f _ -> f o0)
  a <|> b = Layer (\i o x f k -> getLayer a i o x (\_ -> getLayer b i o x f k) k)

instance Monad (Layer i) where
  return a = Layer (\i o _ _ k -> k a i o)
  m >>= g = Layer $ \i0 o0 x f k -> getLayer m i0 o0 x f $ \a i o ->
                                    getLayer (g a) i o x f k

instance MonadPlus (Layer i) where
  mzero = empty
  mplus = (<|>)

instance StateM (Layer i) i where
  get   = Layer (\i0 o0 _ _ k -> k (lsState i0) i0 o0)
  set i = Layer (\i0 o0 _ _ k -> k () (i0 { lsState = i }) o0)

instance BaseM (Layer i) (Layer i) where
  inBase = id


-- Utilities -------------------------------------------------------------------

-- | Finish early, successfully, with no further processing.
finish :: Layer i a
finish  = Layer (\i o x _ _ -> x i o)
{-# INLINE finish #-}

{-# INLINE dropPacket #-}
dropPacket :: Layer i a
dropPacket  = finish

{-# INLINE time #-}
time :: Layer i POSIXTime
time  = Layer (\i o _ _ k -> k (lsNow i) i o)

output :: IO () -> Layer i ()
output m = Layer $ \i0 o0 _ _ k -> k () i0 (o0 `mappend` Action m)

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
