{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}

module Hans.Lens (

    -- * Lenses
    Lens, Lens',
    lens,

    -- ** Getters
    Getting,
    Getter,
    view,
    to,

    -- ** Setters
    ASetter, ASetter',
    set,
    over,
    modify,

    -- * Utility Lenses
    bit,
    byte,

  ) where

import qualified Control.Applicative as A
import qualified Data.Bits as B
import           Data.Word (Word8)
import           MonadLib (Id,runId)


-- Lenses ----------------------------------------------------------------------

-- | General lenses that allow for the type of the inner field to change.
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- | Lenses that don't change type.
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get upd = \ f s -> upd s `fmap` f (get s)
{-# INLINE lens #-}


-- Getters ---------------------------------------------------------------------

type Getting r s a = (a -> Const r a) -> (s -> Const r s)

type Getter s a = forall r. Getting r s a

newtype Const r a = Const { runConst :: r } deriving Functor

-- | This is just a handy way of not exposing a Contrafunctor class, as we only
-- really need it for the definition of `to`.
castConst' :: (b -> a) -> Const r a -> Const r b
castConst' _ (Const r) = Const r
{-# INLINE castConst' #-}

-- NOTE: the @(s -> a)@ part could be generalized to @ReaderM m s@.
view :: Getting a s a -> s -> a
view l = \ s -> runConst (l Const s)
{-# INLINE view #-}

to :: (s -> a) -> Getting r s a
to f = \ l s -> castConst' f (l (f s))
{-# INLINE to #-}


-- Setters ---------------------------------------------------------------------

type ASetter s t a b = (a -> Id b) -> (s -> Id t)

type ASetter' s a = ASetter s s a a

set :: Lens s t a b -> b -> s -> t
set l b = \ s -> runId (l (\ _ -> A.pure b) s)
{-# INLINE set #-}

over :: ASetter s t a b -> (a -> b) -> (s -> t)
over l f = \ s -> runId (l (A.pure . f) s)
{-# INLINE over #-}

newtype Modify r a = Modify { runModify :: (a,r) }

instance Functor (Modify r) where
  fmap f = \ (Modify (a,r)) -> Modify (f a, r)
  {-# INLINE fmap #-}

modify :: Lens s t a b -> (a -> (b,r)) -> (s -> (t,r))
modify l f = \ s -> runModify (l (\ a -> Modify (f a)) s)
{-# INLINE modify #-}


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


byte :: (Integral a, B.Bits a) => Int -> Lens' a Word8
byte n = lens get upd
  where
  sh      = n * 8

  get a   = fromIntegral (a `B.shiftR` sh)

  upd a b = a B..|. (fromIntegral b `B.shiftL` sh)
