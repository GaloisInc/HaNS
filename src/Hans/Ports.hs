{-# LANGUAGE RecordWildCards #-}

module Hans.Ports (
    -- * Port Management
    PortManager
  , emptyPortManager
  , isUsed
  , reserve
  , unreserve
  , nextPort
  ) where

import Control.Monad (guard)
import qualified Data.Set as Set


-- Port Management -------------------------------------------------------------

data PortManager i = PortManager
  { portNext     :: [i]
  , portReserved :: Set.Set i
  , portActive   :: Set.Set i
  }

instance Show i => Show (PortManager i) where
  show pm = "<Active ports: " ++ show (portActive pm) ++ ">"

emptyPortManager :: [i] -> PortManager i
emptyPortManager range = PortManager
  { portNext     = range
  , portReserved = Set.empty
  , portActive   = Set.empty
  }

isUsed :: (Eq i, Ord i) => i -> PortManager i -> Bool
isUsed i PortManager { .. } = i `Set.member` portActive
                           || i `Set.member` portReserved

reserve :: (Eq i, Ord i, Show i) => i -> PortManager i -> Maybe (PortManager i)
reserve i pm = do
  guard (not (isUsed i pm))
  return $! pm
    { portReserved = Set.insert i (portReserved pm)
    }

unreserve :: (Eq i, Ord i, Show i) => i -> PortManager i -> Maybe (PortManager i)
unreserve i pm @ PortManager { .. }
  | Set.member i portReserved = Just pm  { portReserved = Set.delete i portReserved }
  | Set.member i portActive   = Just pm' { portActive   = Set.delete i portActive }
  | otherwise                 = Nothing
  where
  pm' = pm { portNext = i : portNext }

nextPort :: (Eq i, Ord i, Show i) => PortManager i -> Maybe (i, PortManager i)
nextPort pm = case span (`isUsed` pm) (portNext pm) of

  (_,i:rest) -> return (i, pm { portNext = rest
                              , portActive = Set.insert i (portActive pm) })

  _ -> Nothing
