{-# LANGUAGE CPP #-}

module Data.PrefixTree (
    PrefixTree

    -- * Construction
  , empty
  , singleton
  , insert
  , delete
  , toList
  , fromList

    -- * Querying
  , lookup
  , member
  , matches
  , match
  , elems
  , keys
  , key
  ) where

import Prelude hiding (lookup)
import Data.Maybe (isJust,listToMaybe)

#ifdef TESTS
import Data.List (nub)
import Test.QuickCheck
#endif

data PrefixTree a
  = Empty
  | Prefix Key (Maybe a) (PrefixTree a)
  | Branch (PrefixTree a) (PrefixTree a)
    deriving Show

type Key = [Bool]

-- Prefix Manipulation ---------------------------------------------------------

matchPrefix :: Key -> Key -> (Key,Key,Key)
matchPrefix = loop id
  where
  loop k (a:as) (b:bs) | a == b = loop (k . (a:)) as bs
  loop k as     bs              = (k [], as, bs)


-- Construction ----------------------------------------------------------------

empty :: PrefixTree a
empty  = Empty

singleton :: Key -> a -> PrefixTree a
singleton ks a = Prefix ks (Just a) empty

fromList :: [(Key,a)] -> PrefixTree a
fromList  = foldr (uncurry insert) empty

toList :: PrefixTree a -> [([Bool], a)]
toList t =
  case t of
    Empty -> []

    Prefix ls mb t' ->
      case mb of
        Nothing -> map (prefix ls) (toList t')
        Just a  -> (ls,a) : map (prefix ls) (toList t')

    Branch l r -> toList l ++ toList r

  where
  prefix ls (ks,a) = (ls ++ ks, a)

elems :: PrefixTree a -> [a]
elems t =
  case t of
    Empty                -> []
    Prefix _ (Just a) t' -> a : elems t'
    Prefix _ _        t' -> elems t'
    Branch l r           -> elems l ++ elems r

insert :: Key -> a -> PrefixTree a -> PrefixTree a
insert ks a t =
  case t of

    Empty -> singleton ks a

    Prefix ls mb t' ->
      case matchPrefix ks ls of
        -- empty node
        ([],[],[]) -> Prefix [] (Just a) t'

        -- empty key
        ([],[],_) -> Prefix [] (Just a) t

        -- empty node, full key
        ([],_,[]) -> Prefix [] mb (insert ks a t')

        -- no common prefix, branch.
        ([], k:_, _)
          | k         -> Branch (singleton ks a) t
          | otherwise -> Branch t (singleton ks a)

        -- complete match, replace the value
        (_, [], []) -> Prefix ks (Just a) t'

        -- complete prefix match, but partial key match
        (_ ,ks',[]) -> Prefix ls mb (insert ks' a t')

        -- complete key match, partial prefix match
        (_,[],ls') -> Prefix ks (Just a) (Prefix ls' mb t')

        -- partial common prefix, but not the full key
        (ps,ks'@(k:_),ls') -> Prefix ps Nothing br
          where
          t1             = singleton ks' a
          t2             = Prefix ls' mb t'
          br | k         = Branch t1 t2
             | otherwise = Branch t2 t1

    Branch l r ->
      case ks of
        []              -> Prefix [] (Just a) t
        b:_ | b         -> Branch (insert ks a l) r
            | otherwise -> Branch l (insert ks a r)


delete :: Key -> PrefixTree a -> PrefixTree a
delete ks t =
  case t of

    Empty -> Empty

    Prefix ls mb t' ->
      case matchPrefix ks ls of
        (_,[],[])   -> compact (Prefix ls Nothing t')
        ([],ks',[]) -> compact (Prefix ls mb (delete ks' t'))
        _           -> t

    Branch l r ->
      case ks of
        []               -> t
        b:bs | b         -> compact (Branch (delete bs l) r)
             | otherwise -> compact (Branch l (delete bs r))

compact :: PrefixTree a -> PrefixTree a
compact t =
  case t of

    Prefix ls Nothing (Prefix ks mb t') -> Prefix (ls ++ ks) mb t'

    Branch l Empty -> l
    Branch Empty r -> r

    _ -> t


-- Querying --------------------------------------------------------------------

member :: Key -> PrefixTree a -> Bool
member ks t =
  case t of

    Empty -> False

    Prefix ls mb t' ->
      case matchPrefix ks ls of
        (_,[], []) -> isJust mb
        (_,ks',[]) -> member ks' t'
        _          -> False

    Branch l r ->
      case ks of
        []              -> False
        b:_ | b         -> member ks l
            | otherwise -> member ks r

matches :: Key -> PrefixTree a -> [a]
matches = loop []
  where
  loop ms ks t =
    case t of

      Empty -> ms

      Prefix ls mb t' ->
        case matchPrefix ks ls of
          (_,[], []) -> maybe ms (:ms) mb
          (_,ks',[]) -> loop (maybe ms (:ms) mb) ks' t'
          _          -> ms

      Branch l r ->
        case ks of
          []              -> ms
          b:_ | b         -> loop ms ks l
              | otherwise -> loop ms ks r

match :: Key -> PrefixTree a -> Maybe a
match k t = listToMaybe (matches k t)

lookup :: Key -> PrefixTree a -> Maybe a
lookup = match

keys :: Key -> PrefixTree a -> [Key]
keys  = keys' [] []
  where
  keys' as p ks t =
    case t of

      Empty -> as

      Prefix ls _ t' ->
        case matchPrefix ks ls of
          (ps,ks',[]) -> keys' (p':as) p' ks' t'
            where p' = p ++ ps
          _           -> as

      Branch l r -> keys' ls p ks r
        where ls = keys' as p ks l

key :: Key -> PrefixTree a -> Maybe Key
key ks t = listToMaybe (keys ks t)


-- Tests -----------------------------------------------------------------------

#ifdef TESTS
forAllUniqueLists :: (Testable prop, Arbitrary a, Show a, Eq a)
                  => ([a] -> prop) -> Property
forAllUniqueLists = forAll (nub `fmap` arbitrary)

prop_toList_fromList = forAllUniqueLists p
  where
  p :: [([Bool],())] -> Bool
  p bs = length bs == length bs' && all (`elem` bs) bs'
    where bs' = toList (fromList bs)

prop_matchesOrder bs = and (map (f . fst) bs)
  where
  t1  = fromList bs
  t2  = fromList (reverse bs)
  f k = matches k t1 == matches k t2
#endif
