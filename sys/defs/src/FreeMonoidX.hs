{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeMonoidX
-- Copyright   :  (c) TNO and Radboud University
-- License     :  Closed-style (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
--  Free Monoid with a multiplication operation. Symbolic representation of
--  free-monoids of the form:
--
-- > a0 <> a1 <> ... <> an-1
--
-- `ai \in A`, `<>` is a commutative (binary) operation, `(A, <>)` is a
-- Monoid, there is a multiplication operator `<.>` that is distributive
-- w.r.t `<>`, and there is additive inverse for the elements of `A`:
--
-- > a <> b         = b <> a                 -- for all a, b in A (commutativity)
-- > a <.> (b <> c) = (a <.> b) <> (a <.> c) -- for all a, b, c in A (left distributivity).
-- > (b <> c) <.> a = (b <.> a) <> (c <.> a) -- for all a, b, c in A (right distributivity).
-- > a <> -a        = 0                      -- for all a, for some (-a), where `0 = mempty`.
--
-- Furthermore, to be able to assign semantics to this symbolic representation
-- it is required that the terms are integer-multipliable, meaning that
--
-- > `a <.> n`
--
-- is equivalent to adding an element `a` `n` times if `n` is non-negative, or
-- removing it `n` times otherwise. See `IntMultipliable` class, and
-- `multiplyLaw` in `FreeMonoidXSpec`.
--
-- Note that we're using `<.>` as having both types `A -> A` and `Integral n =>
-- n -> a`, which is fine at the moment since the `FreeMonoidX`'s we are
-- dealing with are numeric types.
--
--
-- `FreeMonoidX a` is an instance of the `IsList` class, which means that in
-- combination with the `OverloadedLists` extension is possible to write
-- free-monoids as lists. For instance the free-monoid:
--
-- > a0 <> a1 <> ... <> an-1
--
-- Can be written as
--
-- [a0, a1, ..., an-1]
--
-----------------------------------------------------------------------------

module FreeMonoidX
  ( -- * Free Monoid with multiplication Type
    FreeMonoidX (..)

  -- * Query
  , nrofDistinctTerms
  , distinctTerms  -- exposed for performance reasons checking properties for
                   -- all distinct terms is faster than for all terms
  , distinctTermsT
  -- * Map
  , mapTerms

  -- * Filter
  , partition
  , partitionT

  -- | Folds
  , foldOccur
  , foldFMX

    -- * Manipulation of the free monoid
  , append
  , remove
  , appendMany
  , flatten

  -- * Multiplication operator
  , (<.>)

  -- * Monoid terms restriction
  , IntMultipliable
  , TermWrapper (..)

  -- * Occurrence lists
  , toOccurList
  , toDistinctAscOccurList
  , fromOccurList
  , fromDistinctAscOccurList
  -- ** Occurrence lists of `TermWrapper`'s
  , fromListT
  , toOccurListT
  , toDistinctAscOccurListT
  , fromOccurListT
  , fromDistinctAscPowerListT
  )
where

import           Control.Arrow   (first, (***))
import           Control.DeepSeq
import           Data.Foldable
import           Data.List       hiding (partition)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     hiding (Product (..))
import           GHC.Exts
import           GHC.Generics    (Generic)

-- | Symbolic representation of a polynomial, where each term is a member of
-- type `a`.
--
-- The integer value of the map represents the number of occurrences of a term
-- in the polynomial. Given this representation it is crucial that the
-- operation is commutative, since the information of about the order of the
-- term is lost in this representation.
--
newtype FreeMonoidX a = FMX { asMap :: Map a Integer }
    deriving (Eq, Ord, Read, Generic, NFData)

instance Show a => Show (FreeMonoidX a) where
    show (FMX p) = show (Map.assocs p)

-- | A term of the monoid which wraps a value. This could be for instance a
-- sum-term or a product term.
class TermWrapper f where
    wrap :: a -> f a
    unwrap :: f a -> a

-- | Types that can be multiplied by an integral. This restriction is required
-- to be able to assign a correct semantic to the symbolic representation of
-- the free-monoid. If the elements of the free-monoid are not
-- `IntMultipliable`, then `foldFMX` is ill-defined.
--
-- See the test code `FreeMonoidXspec` for examples of instances of this class.
class IntMultipliable a where
    -- | `n <.> x` multiplies `x` `n` times.
    (<.>) :: Integral n
         => n -- ^ Multiplication factor.
         -> a -- ^ Element to multiply.
         -> a

instance Ord a => IntMultipliable (FreeMonoidX a) where
    0 <.> _ = mempty
    n <.> (FMX p) = FMX $ (toInteger n *) <$> p

instance Ord a => Monoid (FreeMonoidX a) where
    mempty = FMX []
    FMX p0 `mappend` FMX p1 = FMX $ Map.filter (/= 0) $ Map.unionWith (+) p0 p1

instance Ord a => IsList (FreeMonoidX a) where
    type Item (FreeMonoidX a) = a
    fromList xs = FMX $ Map.fromListWith (+) $ zip xs (repeat 1)
    toList (FMX p) = do
        (x, n) <- Map.toList p
        genericReplicate n x

-- | /O(t*log t)/. Create a product from a list of terms using the given term
-- wrapper.
fromListT :: (Ord (t a), TermWrapper t) => [a] -> FreeMonoidX (t a)
fromListT = fromList . (wrap <$>)

-- | Fold the free-monoid.
foldFMX :: (IntMultipliable a, Monoid a) => FreeMonoidX a -> a
foldFMX (FMX p) = Map.foldrWithKey (\x n -> (n <.> x <>)) mempty p

-- | Number of distinct terms in the free-monoid.
nrofDistinctTerms :: FreeMonoidX a -> Int
nrofDistinctTerms = Map.size . asMap

-- | /O(n)/. The distinct terms of a free-monoid., each term occurs only once in
-- the list.
--
distinctTerms :: FreeMonoidX a -> [a]
distinctTerms = Map.keys . asMap

distinctTermsT :: TermWrapper t => FreeMonoidX (t a) -> [a]
distinctTermsT = (unwrap <$>) . distinctTerms

-- | /O(n)/. Convert the free-monoid to a list of (term, occurrence) tuples.
--
-- For instance given the term:
--
-- > a <> b <> a <> b <> a
--
-- `toOccurList` applied to it will result in the following list:
--
-- > [(a, 3), (b, 2)]
--
toOccurList :: FreeMonoidX a -> [(a, Integer)]
toOccurList = toDistinctAscOccurList

toOccurListT :: TermWrapper t => FreeMonoidX (t a) -> [(a, Integer)]
toOccurListT = (first unwrap <$>) . toDistinctAscOccurList

-- | /O(n)/. Convert the free-monoid to a distinct ascending list of term\/multiplier
-- pairs.
toDistinctAscOccurList :: FreeMonoidX a -> [(a, Integer)]
toDistinctAscOccurList = Map.toAscList . asMap

toDistinctAscOccurListT :: TermWrapper t => FreeMonoidX (t a) -> [(a, Integer)]
toDistinctAscOccurListT = (first unwrap <$>) . toDistinctAscOccurList

-- | /O(n*log n)/. Create a free-monoid from a list of term\/multiplier pairs.
fromOccurList :: Ord a => [(a, Integer)] -> FreeMonoidX a
fromOccurList = FMX . Map.filter (0/=) . Map.fromListWith (+)

fromOccurListT :: (Ord (t a), TermWrapper t) => [(a, Integer)] -> FreeMonoidX (t a)
fromOccurListT = fromOccurList . (first wrap <$>)

-- | /O(n)/. Build a free-monoid from an ascending list of term\/multiplier
-- pairs where each term appears only once. /The precondition (input list is
-- strictly ascending) is not checked./
fromDistinctAscOccurList :: [(a, Integer)] -> FreeMonoidX a
fromDistinctAscOccurList = FMX . Map.filter (0/=) . Map.fromDistinctAscList

fromDistinctAscPowerListT :: TermWrapper t => [(a, Integer)] -> FreeMonoidX (t a)
fromDistinctAscPowerListT = fromDistinctAscOccurList . (first wrap <$>)

-- | Append a term to the free-monoid.
--
-- > append 2 [1, 2, 3]
--
-- should be equivalent to the free-monoid.
--
-- > [1, 2, 3, 2]
append :: Ord a => a -> FreeMonoidX a -> FreeMonoidX a
append = appendMany 1

-- | Remove a term from the free-monoid.
--
-- > remove 2 (1 <> 2 <> 3)
--
-- should be equivalent to the free-monoid
--
-- > (1 <> 3)
remove :: Ord a => a -> FreeMonoidX a -> FreeMonoidX a
remove = appendMany (-1)

-- | Add the term `x` `n` times. If `n` is negative the term will be removed
-- `n` times.
--
-- > appendMany 2 10 (10 <> 12 <> 12)
--
-- should be equivalent to
--
-- (10 <> 10 <> 10 <> 12 <> 12)
--
-- > appendMany (-2) 10 (10 <> 12 <> 12)
--
-- should be equivalent to
--
-- > (-10 <> 12 <> 12)
appendMany :: Ord a => Integer -> a -> FreeMonoidX a -> FreeMonoidX a
appendMany 0 _ s = s                                    -- invariant: no term with multiplier 0 is stored.
appendMany m x s = (FMX . Map.alter increment x . asMap) s
    where
        increment :: Maybe Integer -> Maybe Integer
        increment Nothing  = Just m
        increment (Just n) | n == -m = Nothing           -- Terms with multiplier zero are removed
        increment (Just n) = Just (n+m)

-- | /O(n)/. Partition the free-monoid into two free-monoids, one with all
-- elements that satisfy the predicate and one with all elements that don't
-- satisfy the predicate.
partition :: (a -> Bool) -> FreeMonoidX a -> (FreeMonoidX a,FreeMonoidX a)
partition p = (FMX *** FMX) . Map.partitionWithKey (\k _ -> p k) . asMap

partitionT :: TermWrapper t => (a -> Bool) -> FreeMonoidX (t a)
           -> (FreeMonoidX (t a), FreeMonoidX (t a))
partitionT p = partition (p . unwrap)

-- | /O(n)/. Fold over the terms of the free-monoid with their multipliers.
foldOccur :: (a -> Integer -> b -> b) -> b -> FreeMonoidX a -> b
foldOccur f z = Map.foldrWithKey f z . asMap

-- | Map the terms of the free-monoid.
--
mapTerms :: Ord b => (a -> b) -> FreeMonoidX a -> FreeMonoidX b
mapTerms f = fromOccurList . (first f <$>) . toOccurList

-- | Flatten a free-monoid.
--
-- For instance, the monoid
--
-- > (a <> b) <> (a <> b) <> a
--
-- will be rewritten as:
--
-- > a <> a <> a <> b <> b
--
-- Assuming `a < b`.
--
flatten :: (Ord a, IntMultipliable a) => FreeMonoidX (FreeMonoidX a) -> FreeMonoidX a
flatten (FMX p) = fold $ multiplyFMX <$> Map.toAscList p
    where
      multiplyFMX :: Ord a => (FreeMonoidX a, Integer) -> FreeMonoidX a
      multiplyFMX (fm, n) = n <.> fm