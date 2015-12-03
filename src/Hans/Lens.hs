{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}

module Hans.Lens where

import qualified Data.Bits as B


-- Lenses ----------------------------------------------------------------------

-- | General lenses that allow for the type of the inner field to change.
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get upd = \ f s -> upd s `fmap` f (get s)


-- | Lenses that don't change type.
type Lens' s a = Lens s s a a


newtype View b a = Get { runGet :: b } deriving Functor

view :: Lens s t a b -> s -> a
view l = \ s -> runGet (l (\ a -> Get a ) s)
{-# INLINE view #-}


newtype Set a = Set { runSet :: a } deriving Functor

set :: Lens s t a b -> b -> s -> t
set l b = \ s -> runSet (l (\ _ -> Set b) s)
{-# INLINE set #-}


over :: Lens s t a b -> (a -> b) -> (s -> t)
over l f = \ s -> set l (f (view l s)) s
{-# INLINE over #-}


-- Utility Lenses --------------------------------------------------------------

-- NOTE: successive uses of 'bit' with 'set' will cause terms like this to build
-- up:
--
-- > or# (or# .. (or# val (__word m1)) .. (__word mi)) (__word mj)
--
-- This can be fixed with a rewrite rule that associates all uses of or# to the
-- right, but this seems like it might block other simplifications that would
-- have fired if it was associated to the left.
--
-- The real problem here is that GHC isn't reorganizing the uses of or# to group
-- together constants, which would allow only one final use of or# after
-- simplification.
bit :: B.Bits a => Int -> Lens' a Bool
bit n = lens get upd
  where
  get a       = B.testBit a n

  upd a True  = B.setBit   a n
  upd a False = B.clearBit a n
