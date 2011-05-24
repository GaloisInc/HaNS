
module Hans.Ports (
    -- * Port Management
    PortManager
  , emptyPortManager
  , isReserved
  , reserve
  , unreserve
  , nextPort
  ) where

import Control.Monad (MonadPlus(mzero),guard)
import Data.List (delete)
import qualified Data.Set as Set


-- Port Management -------------------------------------------------------------

data PortManager i = PortManager
  { portNext   :: [i]
  , portActive :: Set.Set i
  }

emptyPortManager :: [i] -> PortManager i
emptyPortManager range = PortManager
  { portNext   = range
  , portActive = Set.empty
  }

isReserved :: (Eq i, Ord i) => i -> PortManager i -> Bool
isReserved i pm = i `Set.member` portActive pm

reserve :: (MonadPlus m, Eq i, Ord i) => i -> PortManager i -> m (PortManager i)
reserve i pm = do
  guard (not (isReserved i pm))
  return $! pm
    { portNext   = delete i (portNext pm)
    , portActive = Set.insert i (portActive pm)
    }

unreserve :: (MonadPlus m, Eq i, Ord i)
          => i -> PortManager i -> m (PortManager i)
unreserve i pm = do
  guard (isReserved i pm)
  return $! pm
    { portNext   = i : portNext pm
    , portActive = Set.delete i (portActive pm)
    }

nextPort :: (MonadPlus m, Eq i, Ord i)
         => PortManager i -> m (i, PortManager i)
nextPort pm = case portNext pm of
  []  -> mzero
  i:_ -> do
    pm' <- reserve i pm
    return $! (i,pm')
