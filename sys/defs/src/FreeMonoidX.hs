{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS -Wall -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BopPolynomial
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
-- Monoid, and there is a multiplication operator `**` that is distributive
-- w.r.t `<>`:
--
-- > a <> b        = b <> a               -- for all a, b in A (commutativity)
-- > a ** (b <> c) = (a ** b) <> (a ** c) -- for all a, b, c in A (left distributivity).
-- > (b <> c) ** a = (b ** a) <> (c ** a) -- for all a, b, c in A (right distributivity).
--
-----------------------------------------------------------------------------

module FreeMonoidX
  ( -- * Binary Operation Polynomial Type.
    FreeMonoidX (..)

    -- * Query.
  , nrofDistinctTerms
  , distinctTerms  -- exposed for performance reasons checking properties for
                   -- all distinct terms is faster than for all terms

  -- * Filter
  , partition

  -- | Folds
  , foldMultiplier
  , foldP

    -- * Manipulation of the polynomial.
  , append
  , remove
  , addNTimes

  -- * Application of the binary operation.
  , multiply

  -- * Class that define restrictions on the polynomial types.
  , IntMultipliable

  -- ** Multiplier lists
  , toMultiplierList
  , toDistinctAscMultiplierList
  , fromMultiplierList
  , fromDistinctAscMultiplierList
  )
where

import           Control.Arrow   ((***))
import           Data.List       hiding (partition)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     hiding (Product (..))
import           GHC.Exts

-- | Symbolic representation of a polynomial, where each term is a member of
-- type `a`.
--
-- The integer value of the map represents the number of occurrences of a term
-- in the polynomial. Given this representation it is crucial that the
-- operation is commutative, since the information of about the order of the
-- term is lost in this representation.
--
newtype FreeMonoidX a = FPX { asMap :: Map a Integer }
    deriving (Eq)

instance Show a => Show (FreeMonoidX a) where
    show (FPX p) = show (Map.assocs p)

-- | Types that can be multiplied by an integral. See the test code
-- `FreeMonoidXspec` for examples of instances of this class.
class IntMultipliable a where
    -- | `multiply n x` multiplies `x` `n` times.
    multiply :: Integral n
             => n -- ^ Multiplication factor.
             -> a -- ^ Element to multiply.
             -> a

instance IntMultipliable (FreeMonoidX a) where
    multiply n (FPX p) = FPX $ (toInteger n *) <$> p

instance Ord a => Monoid (FreeMonoidX a) where
    mempty = FPX []
    FPX p0 `mappend` FPX p1 = FPX $ Map.filter (/= 0) $ Map.unionWith (+) p0 p1

instance Ord a => IsList (FreeMonoidX a) where
    type Item (FreeMonoidX a) = a
    fromList xs = FPX $ Map.fromListWith (+) $ zip xs (repeat 1)
    toList (FPX p) = do
        (x, n) <- Map.toList p
        genericReplicate n x

-- | Fold the polynomial.
foldP :: (IntMultipliable a, Monoid a) => FreeMonoidX a -> a
foldP (FPX p) = Map.foldrWithKey (\x n -> (multiply n x <>)) mempty p

-- | Number of distinct terms in the polynomial.
nrofDistinctTerms :: FreeMonoidX a -> Int
nrofDistinctTerms = Map.size . asMap

-- | /O(n)/. The distinct terms of a polynomial., each term occurs only once in
-- the list.
--
distinctTerms :: FreeMonoidX a -> [a]
distinctTerms = Map.keys . asMap

-- | /O(n)/. Convert the polynomial to a list of term\/multiplier pairs.
toMultiplierList :: FreeMonoidX a -> [(a, Integer)]
toMultiplierList = toDistinctAscMultiplierList

-- | /O(n)/. Convert the polynomial to a distinct ascending list of term\/multiplier
-- pairs.
toDistinctAscMultiplierList :: FreeMonoidX a -> [(a, Integer)]
toDistinctAscMultiplierList = Map.toAscList . asMap

-- | /O(n*log n)/. Create a polynomial from a list of term\/multiplier pairs.
fromMultiplierList :: Ord a => [(a, Integer)] -> FreeMonoidX a
fromMultiplierList = FPX . Map.filter (0/=) . Map.fromListWith (+)

-- | /O(n)/. Build a polynomial from an ascending list of term\/multiplier
-- pairs where each term appears only once. /The precondition (input list is
-- strictly ascending) is not checked./
fromDistinctAscMultiplierList :: [(a, Integer)] -> FreeMonoidX a
fromDistinctAscMultiplierList = FPX . Map.filter (0/=) . Map.fromDistinctAscList

-- | Append a term to the polynomial.
--
-- > append 2 [1, 2, 3]
--
-- should be equivalent to the polynomial
--
-- > [1, 2, 3, 2]
append :: Ord a => a -> FreeMonoidX a -> FreeMonoidX a
append = addNTimes 1

-- | Remove a term from the polynomial.
--
-- > remove 2 [1, 2, 3]
--
-- should be equivalent to the polynomial
--
-- > [1, 3]
remove :: Ord a => a -> FreeMonoidX a -> FreeMonoidX a
remove = addNTimes (-1)

-- | Add the term `x` `n` times. If `n` is negative the term will be removed
-- `n` times.
addNTimes :: Ord a => Integer -> a -> FreeMonoidX a -> FreeMonoidX a
addNTimes 0 _ s = s                                    -- invariant: no term with multiplier 0 is stored.
addNTimes m x s = (FPX . Map.alter increment x . asMap) s
    where
        increment :: Maybe Integer -> Maybe Integer
        increment Nothing  = Just m
        increment (Just n) | n == -m = Nothing           -- Terms with multiplier zero are removed
        increment (Just n) = Just (n+m)

-- | /O(n)/. Partition the polynomial into two polynomial, one with all
-- elements that satisfy the predicate and one with all elements that don't
-- satisfy the predicate.
partition :: (a -> Bool) -> FreeMonoidX a -> (FreeMonoidX a,FreeMonoidX a)
partition p = (FPX *** FPX) . Map.partitionWithKey (\k _ -> p k) . asMap

-- | /O(n)/. Fold over the terms of the polynomial with their multipliers.
foldMultiplier :: (a -> Integer -> b -> b) -> b -> FreeMonoidX a -> b
foldMultiplier f z = Map.foldrWithKey f z . asMap
