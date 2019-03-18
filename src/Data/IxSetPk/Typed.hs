{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.IxSetPk.Typed
  ( module Data.IxSetPk.Typed
  , module X
  ) where

import           Data.IxSet.Typed    as X (IxSet, ixFun, ixGen, ixList)
import qualified Data.IxSet.Typed    as IX
import qualified Data.IxSet.Typed.Ix as Ix
import qualified Data.List           as List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Proxy          as X (Proxy (..))
import           Data.Set            (Set)
import qualified Data.Set            as Set



-- | FIXME: what should I put in here?
data IxSetPk (pkix :: *) (ixs :: [*]) (a :: *) where
  IxSetPk :: !(IxSet ixs a) -> !(Pk pkix a) -> IxSetPk pkix ixs a

instance (Show a, IX.Indexable ixs a) => Show (IxSetPk pkix ixs a) where
  show (IxSetPk ixset _) = show ixset

-- | 'Pk' is a primary key - a 'Map' from some key 'k' to a value 'a'
data Pk (k :: *) (a :: *) where
  Pk :: !(Map k a) -> (a -> k) -> Pk k a


-- | FIXME: change name
class (Ord k, Ord a) => IsPkIx k a where
  pkIndex :: Pk k a

empty :: forall pkix ixs a. (IsPkIx pkix a, IX.Indexable ixs a)
      => IxSetPk pkix ixs a
empty = IxSetPk IX.empty pkIndex

pkFun :: Ord k => (a -> k) -> Pk k a
pkFun = Pk Map.empty

type PkOp = forall k a. (Ord k, Ord a) => k -> a -> Map k a -> Map k a

-- | This is internal function, not exposed to the outside world
change :: forall ixs pkix a. (IsPkIx pkix a, IX.Indexable ixs a)
       => IX.SetOp
       -> IX.IndexOp
       -> PkOp
       -> a
       -> IxSetPk pkix ixs a
       -> IxSetPk pkix ixs a
change opS opI opPk x (IxSetPk ixset pkix) =
  IxSetPk (IX.change opS opI x ixset) (update pkix)
  where
    update (Pk m pkfun) = Pk (opPk (pkfun x) x m) pkfun


insert :: (IsPkIx pkix a, IX.Indexable ixs a)
       => a -> IxSetPk pkix ixs a -> Maybe (IxSetPk pkix ixs a)
insert v ixset@(IxSetPk _ (Pk ix ixfun)) = case Map.lookup (ixfun v) ix of
  Nothing -> Just $ change Set.insert Ix.insert Map.insert v ixset
  Just _  -> Nothing

upsert :: (IsPkIx pkix a, IX.Indexable ixs a)
       => a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
upsert v ixset@(IxSetPk _ (Pk _ ixfun)) =
  change Set.insert Ix.insert Map.insert v $ deletePk (ixfun v) ixset

deletePk :: (IsPkIx pkix a, IX.Indexable ixs a)
         => pkix -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
deletePk pk ixset = case lookupPk pk ixset of
  Nothing -> ixset
  Just v  -> change Set.delete Ix.delete (\x _ m -> Map.delete x m) v ixset

lookupPk :: (IsPkIx pkix a, IX.Indexable ixs a)
         => pkix -> IxSetPk pkix ixs a -> Maybe a
lookupPk pk (IxSetPk _ (Pk ix _)) = Map.lookup pk ix


fromList :: (IsPkIx pkix a, IX.Indexable ixs a) => [a] -> IxSetPk pkix ixs a
fromList xs = List.foldl' (flip upsert) empty xs

toList :: IxSetPk pkix ixs a -> [a]
toList (IxSetPk ixs _) = IX.toList ixs


toIxSet :: IxSetPk pkix ixs a -> IxSet ixs a
toIxSet (IxSetPk ixs _) = ixs

fromIxSet :: (IsPkIx pkix a, IX.Indexable ixs a)
          => IxSet ixs a -> IxSetPk pkix ixs a
fromIxSet = fromSet . IX.toSet


toSet :: IxSetPk pkix ixs a -> Set a
toSet (IxSetPk ixs _) = IX.toSet ixs

fromSet :: (IsPkIx pkix a, IX.Indexable ixs a)
        => Set a -> IxSetPk pkix ixs a
fromSet = fromList . Set.toList

toMap :: IxSetPk pkix ixs a -> Map pkix a
toMap (IxSetPk _ (Pk pkix _)) = pkix

fromMap :: (IsPkIx pkix a, IX.Indexable ixs a)
        => Map pkix a -> IxSetPk pkix ixs a
fromMap = fromList . Map.elems

-- | Return 'True' if the 'IxSetPk' is empty, 'False' otherwise.
null :: IxSetPk pkix ixs a -> Bool
null (IxSetPk ixset _) = IX.null ixset

-- | Returns the number of items in the 'IxSetPk'.
size :: IxSetPk pkix ixs a -> Int
size (IxSetPk ixset _) = IX.size ixset

-- | Converts an 'IxSetPk' to its list of elements.
--
-- List will be sorted in ascending order by the index 'ix'.
--
-- The list will NOT contain duplicate entries.
toAscList :: IX.IsIndexOf ix ixs => Proxy ix -> IxSetPk pkix ixs a -> [a]
toAscList p (IxSetPk ixset _) = IX.toAscList p ixset

-- | Converts an 'IxSetPk' to its list of elements.
--
-- List will be sorted in descending order by the index 'ix'.
--
-- The list will NOT contain duplicate entries.
toDescList :: IX.IsIndexOf ix ixs => Proxy ix -> IxSetPk pkix ixs a -> [a]
toDescList p (IxSetPk ixset _) = IX.toDescList p ixset

--------------------------------------------------------------------------
--                            Query operations
--------------------------------------------------------------------------

-- | Infix version of 'getEQ'.
(@=) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
     => IxSetPk pkix ixs a -> ix -> IxSet ixs a
ix @= v = getEQ v ix

-- | Infix version of 'getLT'.
(@<) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
     => IxSetPk pkix ixs a -> ix -> IxSet ixs a
ix @< v = getLT v ix

-- | Infix version of 'getGT'.
(@>) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
     => IxSetPk pkix ixs a -> ix -> IxSet ixs a
ix @> v = getGT v ix

-- | Infix version of 'getLTE'.
(@<=) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
      => IxSetPk pkix ixs a -> ix -> IxSet ixs a
ix @<= v = getLTE v ix

-- | Infix version of 'getGTE'.
(@>=) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
      => IxSetPk pkix ixs a -> ix -> IxSet ixs a
ix @>= v = getGTE v ix

-- | Returns the subset with indices in the open interval (k,k).
(@><) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
      => IxSetPk pkix ixs a -> (ix, ix) -> IxSet ixs a
ix @>< (v1,v2) = IX.getLT v2 $ getGT v1 ix

-- | Returns the subset with indices in [k,k).
(@>=<) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
       => IxSetPk pkix ixs a -> (ix, ix) -> IxSet ixs a
ix @>=< (v1,v2) = IX.getLT v2 $ getGTE v1 ix

-- | Returns the subset with indices in (k,k].
(@><=) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
       => IxSetPk pkix ixs a -> (ix, ix) -> IxSet ixs a
ix @><= (v1,v2) = IX.getLTE v2 $ getGT v1 ix

-- | Returns the subset with indices in [k,k].
(@>=<=) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
        => IxSetPk pkix ixs a -> (ix, ix) -> IxSet ixs a
ix @>=<= (v1,v2) = IX.getLTE v2 $ getGTE v1 ix

-- | Creates the subset that has an index in the provided list.
(@+) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
     => IxSetPk pkix ixs a -> [ix] -> IxSet ixs a
IxSetPk ixset _ @+ list = ixset IX.@+ list

-- | Creates the subset that matches all the provided indices.
(@*) :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
     => IxSetPk pkix ixs a -> [ix] -> IxSet ixs a
IxSetPk ixset _ @* list = ixset IX.@* list


-- | Returns the subset with an index equal to the provided key.  The
-- set must be indexed over key type.
getEQ :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
      => ix -> IxSetPk pkix ixs a -> IxSet ixs a
getEQ v (IxSetPk ixset _) = IX.getEQ v ixset

getLT :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
       => ix -> IxSetPk pkix ixs a -> IxSet ixs a
getLT v (IxSetPk ixset _) = IX.getLT v ixset

getGT :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
      => ix -> IxSetPk pkix ixs a -> IxSet ixs a
getGT v (IxSetPk ixset _) = IX.getGT v ixset

getLTE :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
       => ix -> IxSetPk pkix ixs a -> IxSet ixs a
getLTE v (IxSetPk ixset _) = IX.getLTE v ixset

getGTE :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
       => ix -> IxSetPk pkix ixs a -> IxSet ixs a
getGTE v (IxSetPk ixset _) = IX.getGTE v ixset

-- | Returns the subset with an index within the interval provided.
-- The bottom of the interval is closed and the top is open,
-- i. e. [k1;k2).
getRange :: (IX.Indexable ixs a, IX.IsIndexOf ix ixs)
         => ix -> ix -> IxSetPk pkix ixs a -> IxSet ixs a
getRange k1 k2 (IxSetPk ixset _) = IX.getGTE k1 (IX.getLT k2 ixset)

-- | Returns lists of elements paired with the indices determined by
-- type inference.
groupBy :: forall pkix ix ixs a. IX.IsIndexOf ix ixs
        => IxSetPk pkix ixs a -> [(ix, [a])]
groupBy (IxSetPk ixset _) = IX.groupBy ixset


-- | Returns lists of elements paired with the indices determined by
-- type inference.
--
-- The resulting list will be sorted in ascending order by 'ix'.
-- The values in @[a]@ will be sorted in ascending order as well.
groupAscBy :: forall pkix ix ixs a. IX.IsIndexOf ix ixs
           => IxSetPk pkix ixs a -> [(ix, [a])]
groupAscBy (IxSetPk ixset _) = IX.groupAscBy ixset

-- | Returns lists of elements paired with the indices determined by
-- type inference.
--
-- The resulting list will be sorted in descending order by 'ix'.
--
-- NOTE: The values in @[a]@ are currently sorted in ascending
-- order. But this may change if someone bothers to add
-- 'Set.toDescList'. So do not rely on the sort order of the
-- resulting list.
groupDescBy :: forall pkix ix ixs a. IX.IsIndexOf ix ixs
           => IxSetPk pkix ixs a -> [(ix, [a])]
groupDescBy (IxSetPk ixset _) = IX.groupDescBy ixset

--------------------------------------------------------------------------
-- Set operations
--------------------------------------------------------------------------

-- | An infix 'intersection' operation.
(&&&) :: (IsPkIx pkix a, IX.Indexable ixs a)
      => IxSetPk pkix ixs a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
(&&&) = intersection

-- | An infix 'union' operation.
(|||) :: (IsPkIx pkix a, IX.Indexable ixs a)
      => IxSetPk pkix ixs a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
(|||) = union

-- | An infix 'difference' operation.
(\\\) :: (IsPkIx pkix a, IX.Indexable ixs a)
      => IxSetPk pkix ixs a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
(\\\) = difference

infixr 5 &&&
infixr 5 |||
infixr 5 \\\

-- | Takes the union of the two 'IxSetPk's. It follows Map's semantics
--   (it is based on primary keys, rather than values).
union :: (IsPkIx pkix a, IX.Indexable ixs a)
      => IxSetPk pkix ixs a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
union xs1 xs2 = fromMap $ toMap xs1 `Map.union` toMap xs2
-- FIXME: implementation may not be optimal...

-- | Takes the intersection of the two 'IxSetPk's. It follows Map's semantics
--   (it is based on primary keys, rather than values).
intersection :: (IsPkIx pkix a, IX.Indexable ixs a)
             => IxSetPk pkix ixs a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
intersection xs1 xs2 = fromMap $ toMap xs1 `Map.intersection` toMap xs2
-- FIXME: implementation may not be optimal...

-- | Takes the difference of the two 'IxSetPk's. It follows Map's semantics
--   (it is based on primary keys, rather than values).
difference :: (IsPkIx pkix a, IX.Indexable ixs a)
           => IxSetPk pkix ixs a -> IxSetPk pkix ixs a -> IxSetPk pkix ixs a
difference xs1 xs2 = fromMap $ toMap xs1 `Map.difference` toMap xs2
-- FIXME: implementation may not be optimal...

