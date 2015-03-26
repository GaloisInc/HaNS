
module Hans.Layer.IP4.Fragmentation where

import Hans.Address
import Hans.Address.IP4
import Hans.Message.Ip4
import Hans.Utils (chunk)

import Data.Ord (comparing)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict      as Map
import qualified Data.ByteString      as S


type FragmentationTable addr = Map.Map (Ident,addr,addr) Fragments

emptyFragmentationTable :: FragmentationTable IP4
emptyFragmentationTable  = Map.empty


data Fragments = Fragments
  { startTime :: !POSIXTime
  , totalSize :: {-# UNPACK #-} !Int
  , fragments :: ![Fragment]
  } deriving Show

data Fragment = Fragment
  { fragmentOffset  :: {-# UNPACK #-} !Int
  , fragmentLength  :: {-# UNPACK #-} !Int
  , fragmentPayload :: !L.ByteString
  } deriving (Eq,Show)

instance Ord Fragment where
  compare = comparing fragmentOffset


-- | The end of a fragment.
fragmentEnd :: Fragment -> Int
fragmentEnd f = fragmentOffset f + fragmentLength f

-- | Check the ordering of two fragments.
comesBefore :: Fragment -> Fragment -> Bool
comesBefore f g = fragmentEnd f == fragmentOffset g

-- | Check the ordering of two fragments.
comesAfter :: Fragment -> Fragment -> Bool
comesAfter  = flip comesBefore

-- | Merge two fragments.
--
-- Note: This doesn't do a validity check to make sure that they're actually
-- adjacent.
combineFragments :: Fragment -> Fragment -> Fragment
combineFragments f g = Fragment (fragmentOffset f) len pay
  where
  len = fragmentLength f + fragmentLength g
  pay = fragmentPayload f `L.append` fragmentPayload g


-- | Given a group of fragments, a new fragment, and a possible total size,
-- create a new group of fragments that incorporates the new fragment.
expandGroup :: Fragments -> Fragment -> Int -> Fragments
expandGroup fs newfrag x = case totalSize fs of
  -1 | x >= 0 -> expandGroup fs{ totalSize = x } newfrag x
  _           -> fs { fragments = addFragment newfrag (fragments fs) }


-- | Add a fragment to a list of fragments, in a position that is relative to
-- its offset and length.
addFragment :: Fragment -> [Fragment] -> [Fragment]
addFragment f fs = case fs of
  []                         -> [f]
  g:rest | f `comesBefore` g -> addFragment (combineFragments f g) rest
         | f `comesAfter`  g -> addFragment (combineFragments g f) rest
         | f < g             -> f:fs
         | otherwise         -> g:(addFragment f rest)


-- | Process a packet fragment through the system, potentially returning a
-- fully-processed packet if this fragment completes an existing packet or
-- is itself a fully-complete packet.
processFragment :: Address addr
                => POSIXTime -> FragmentationTable addr -> Bool -> Int
                -> addr -> addr -> Ident -> S.ByteString
                -> (FragmentationTable addr, Maybe L.ByteString)
processFragment _   table False   0   _   _    _     bs =
  (table, Just (chunk bs))
processFragment now table areMore off src dest ident bs =
  case group of
    Fragments _ x [Fragment 0 y bs']
      | x == y -> (Map.delete entry table, Just bs')
    _          -> (Map.insert entry group table, Nothing)
  where
  entry = (ident,src,dest)
  group = case Map.lookup (ident,src,dest) table of
    Nothing -> Fragments now newTotalLen [cur]
    Just g  -> expandGroup g cur newTotalLen
  curlen = S.length bs
  cur    = Fragment off curlen (chunk bs)
  newTotalLen | areMore   = -1
              | otherwise = off + curlen


processIP4Packet :: POSIXTime -> FragmentationTable IP4
                 -> IP4Header -> S.ByteString
                 -> (FragmentationTable IP4, Maybe L.ByteString)
processIP4Packet now table hdr bs =
  processFragment now table areMore off src dest ident bs
  where
  off     = fromIntegral (ip4FragmentOffset hdr)
  ident   = fromIntegral (ip4Ident          hdr)
  areMore = ip4MoreFragments hdr
  src     = ip4SourceAddr    hdr
  dest    = ip4DestAddr      hdr
