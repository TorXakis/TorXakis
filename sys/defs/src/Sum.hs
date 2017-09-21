{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sum
-- Copyright   :  (c) TNO and Radboud University
-- License     :  Closed-style (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation for a symbolic sum.
--
-- inspiration taken from
-- https://hackage.haskell.org/package/multiset-0.3.3/docs/src/Data-MultiSet.html
--
-- In the complexity of functions /n/ refers to the number of distinct terms,
-- /t/ is the total number of terms.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
module Sum  (
    -- * Sum type
      Sum

    -- * Query
    , nrofTerms
    , distinctTerms     -- exposed for performance reasons
                        -- checking properties for all distinct terms is faster than for all terms

    -- * Filter
    , partition

    -- * Fold
    , foldMultiplier

    -- * Sum of Term and Sums
    , add
    , subtract
    , addMultiply
    , sum
    , sums


    -- * Multiply
    , multiply

    -- * Constructors and conversion

    -- ** List
    , fromList

    -- ** Multiplier lists
    , toMultiplierList
    , toDistinctAscMultiplierList
    , fromMultiplierList
    , fromDistinctAscMultiplierList
) where

import           Prelude         hiding (subtract, sum)

import           BopPolynomial   (BopPolynomial, Commutative, IntMultipliable)
import qualified BopPolynomial   as BP
import           Control.Arrow   ((***))
import           Control.DeepSeq
import           Data.Foldable   hiding (sum)
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid     as M
import           GHC.Generics    (Generic)

{--------------------------------------------------------------------
  The data type
--------------------------------------------------------------------}
-- | Sum is a parameterized data type.
-- Sum represents a symbolic sum of terms of the type parameter @a@.
-- The same term can occur multiple times.
newtype Sum a = Sum { unSum :: Map.Map a Integer }
    deriving (Eq, Ord, Read, Show, Generic, NFData)

newtype SumPolynomial a = SP { asPolynomial :: BopPolynomial a}

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. The number of distinct terms in the sum.
-- TODO: remove this, use BopPolynomial
nrofTerms :: Sum a -> Int
nrofTerms = Map.size . unSum

nrofTermsNew :: SumPolynomial a -> Int
nrofTermsNew = BP.nrofDistinctTerms . asPolynomial


{--------------------------------------------------------------------
  Sums and multiplications
--------------------------------------------------------------------}
-- | /O(log n)/. Add a term to a sum.
add :: Ord a => a -> Sum a -> Sum a
add t = addMultiply t 1

addNew :: Ord a => a -> SumPolynomial a -> SumPolynomial a
addNew t (SP bp) = SP (BP.append t bp)

-- | /O(log n)/. Subtract a term from a sum.
subtract :: Ord a => a -> Sum a -> Sum a
subtract t = addMultiply t (-1)

subtractNew :: Ord a => a -> SumPolynomial a -> SumPolynomial a
subtractNew t (SP bp) = SP (BP.remove t bp)

-- | /O(log n)/. Add a term multiplied by the given coefficient to a sum.
addMultiply :: Ord a => a -> Integer -> Sum a -> Sum a
addMultiply _ 0 s = s                                    -- invariant: no term with multiplier 0 is stored.
addMultiply x m s = (Sum . Map.alter increment x . unSum) s
    where
        increment :: Maybe Integer -> Maybe Integer
        increment Nothing  = Just m
        increment (Just n) | n == -m = Nothing           -- Terms with multiplier zero are removed
        increment (Just n) = Just (n+m)

-- | The sum of a list of sums.
sums :: Ord a => [Sum a] -> Sum a
sums = List.foldl' sum (Sum Map.empty)

sumsNew :: (Ord a) => [SumPolynomial a] -> SumPolynomial a
sumsNew xs = SP (fold (map asPolynomial xs))

-- | /O(n+m)/. The sum of two sums.
--
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigsum `sum` smallsum).
sum :: Ord a => Sum a -> Sum a -> Sum a
sum (Sum m1) (Sum m2) = Sum $ Map.filter (0/=) $ Map.unionWith (+) m1 m2

-- | /O(n)/. Multiply the constant with the sum.
multiply :: Integer -> Sum a -> Sum a
multiply 0 _ = Sum Map.empty
multiply n s = (Sum . Map.map (n *) . unSum) s

{--------------------------------------------------------------------
  Partition
--------------------------------------------------------------------}
-- | /O(n)/. Partition the sum into two sums, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
partition :: (a -> Bool) -> Sum a -> (Sum a,Sum a)
partition p = (Sum *** Sum) . Map.partitionWithKey (\k _ -> p k) . unSum

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

-- | /O(n)/. Fold over the terms of a sum with their multipliers.
foldMultiplier :: (a -> Integer -> b -> b) -> b -> Sum a -> b
foldMultiplier f z = Map.foldrWithKey f z . unSum

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n)/. The distinct terms of a sum,
-- each term occurs only once in the list.
--
-- > distinctTerms = map fst . toOccurList
distinctTerms :: Sum a -> [a]
distinctTerms = Map.keys . unSum

-- | /O(t*log t)/. Create a sum from a list of terms.
fromList :: Ord a => [a] -> Sum a
fromList xs = fromMultiplierList $ zip xs (repeat 1)

{--------------------------------------------------------------------
  Multiplier lists
--------------------------------------------------------------------}

-- | /O(n)/. Convert the sum to a list of term\/multiplier pairs.
toMultiplierList :: Sum a -> [(a, Integer)]
toMultiplierList = toDistinctAscMultiplierList

-- | /O(n)/. Convert the sum to a distinct ascending list of term\/multiplier pairs.
toDistinctAscMultiplierList :: Sum a -> [(a, Integer)]
toDistinctAscMultiplierList = Map.toAscList . unSum

-- | /O(n*log n)/. Create a sum from a list of term\/multiplier pairs.
fromMultiplierList :: Ord a => [(a, Integer)] -> Sum a
fromMultiplierList = Sum . Map.filter (0/=) . Map.fromListWith (+)

-- | /O(n)/. Build a sum from an ascending list of term\/multiplier pairs where
-- each term appears only once.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscMultiplierList :: [(a, Integer)] -> Sum a
fromDistinctAscMultiplierList = Sum . Map.filter (0/=) . Map.fromDistinctAscList
