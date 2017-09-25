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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall -Werror #-}
module Sum
    (  -- * Sum types
      FreeSum
    , SumTerm (..)

    -- * Query
    , distinctTerms     -- exposed for performance reasons
                        -- checking properties for all distinct terms is faster than for all terms

    -- * Filter
    , partition

    -- * Sum of Term and Sums
    , add
    , subtract
    , sum
    , sums

    -- * Multiply
    , multiply

    -- ** Multiplier lists
    , fromList
    , fromMultiplierList
    , toMultiplierList
    ) where

import           Prelude         hiding (subtract, sum)

import           Control.Arrow   (first)
import           Control.DeepSeq
import           Data.Foldable   hiding (sum)
import           Data.Monoid     ((<>))
import           FreeMonoidX     (FreeMonoidX, IntMultipliable, (<.>))
import qualified FreeMonoidX     as FMX
import qualified GHC.Exts        as Exts
import           GHC.Generics    (Generic)

{--------------------------------------------------------------------
  The data types
--------------------------------------------------------------------}
-- | `FreeSum` represents a symbolic sum of terms of the type parameter `a`.
-- The same term can occur multiple times.
--
type FreeSum a = FreeMonoidX (SumTerm a)

-- | Terms of a free-monoids of the form:
--
-- > a0 <> a1 <> ... <> an-1
--
-- where `<>` will be interpreted as the arithmetic sum of terms:
--
-- > a0 + a1 + ... + an-1
--
newtype SumTerm a = SumTerm { summand :: a }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Functor)

instance Num a => Monoid (SumTerm a) where
    mempty = SumTerm 0
    (SumTerm x) `mappend` (SumTerm y) = SumTerm $ x + y

instance Integral a => IntMultipliable (SumTerm a) where
    n <.> SumTerm x = SumTerm (fromInteger $ toInteger x * toInteger n)

{--------------------------------------------------------------------
  Sums and multiplications
--------------------------------------------------------------------}
-- | /O(log n)/. Add a term to a sum.
add :: Ord a => a -> FreeSum a -> FreeSum a
add = FMX.append . SumTerm

-- | /O(log n)/. Subtract a term from a sum.
subtract :: Ord a => a -> FreeSum a -> FreeSum a
subtract = FMX.remove . SumTerm

-- | The sum of a list of sums.
sums :: (Ord a) => [FreeSum a] -> FreeSum a
sums = fold

-- | /O(n+m)/. The sum of two sums.
sum :: Ord a => FreeSum a -> FreeSum a -> FreeSum a
sum = (<>)

-- | /O(n)/. Multiply the constant with the sum.
multiply :: Ord a => Integer -> FreeSum a -> FreeSum a
multiply = (<.>)

{--------------------------------------------------------------------
  Partition
--------------------------------------------------------------------}
-- | /O(n)/. Partition the sum into two sums, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
partition :: (a -> Bool) -> FreeSum a -> (FreeSum a, FreeSum a)
partition p = FMX.partition (p . summand)

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n)/. The distinct terms of a sum,
-- each term occurs only once in the list.
--
-- > distinctTerms = map fst . toOccurList
distinctTerms :: FreeSum a -> [a]
distinctTerms = (summand <$>) . FMX.distinctTerms

-- | /O(t*log t)/. Create a sum from a list of terms.
fromList :: Ord a => [a] -> FreeSum a
fromList = Exts.fromList . (SumTerm <$>)

{--------------------------------------------------------------------
  Multiplier lists
--------------------------------------------------------------------}
fromMultiplierList :: Ord a => [(a, Integer)] -> FreeSum a
fromMultiplierList = FMX.fromMultiplierList . (first SumTerm <$>)

-- | /O(n)/. Convert the sum to a list of term\/multiplier pairs.
toMultiplierList :: FreeSum  a -> [(a, Integer)]
toMultiplierList = (first summand <$>) . FMX.toDistinctAscMultiplierList
