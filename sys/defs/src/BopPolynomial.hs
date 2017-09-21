{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeFamilies               #-}
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
--  Binary operation polynomials: a symbolic representation of polynomials of
--  the form:
--
-- > a0 <> a1 <> ... <> an-1
--
-- `ai \in A`, and <> is a commutative (binary) operation, and (A, <>) is a
-- Monoid.
-----------------------------------------------------------------------------

module BopPolynomial
  ( -- * Binary Operation Polynomial Type.
    BopPolynomial (..)

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


  -- * Product wrapper, and operations.
  , Product(..)

  -- * Class that define restrictions on the polynomial types.
  , Commutative
  , IntMultipliable

  -- ** Multiplier lists
  , toMultiplierList
  , toDistinctAscMultiplierList
  , fromMultiplierList
  , fromDistinctAscMultiplierList
  )
where

import           Control.Arrow   ((***))
import           Data.AEq
import           Data.List       hiding (partition)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     hiding (Product (..))
import           GHC.Exts

newtype BopPolynomial a = BP { asMap :: Map a Integer }
    deriving (Eq)

instance Show a => Show (BopPolynomial a) where
    show (BP p) = show (Map.assocs p)

-- | Types with a commutative operator.
class Commutative a

-- | Types that can be multiplied by an integral.
class IntMultipliable a where
    -- | `multiply n x` multiplies `x` `n` times.
    multiply :: Integral n
             => n -- ^ Multiplication factor.
             -> a -- ^ Element to multiply.
             -> a

instance Integral a => IntMultipliable (Sum a) where
    multiply n (Sum x) = Sum (fromInteger $ toInteger x * toInteger n)

newtype Product a = Product {getProduct :: a} deriving (Show, Functor, Num, Ord)

instance AEq a => Eq (Product a) where
    (Product x) == (Product y) = x ~== y

instance Num a => Monoid (Product a) where
    mempty = Product 1
    (Product x) `mappend` (Product y) = Product $ x * y

instance Fractional a => Fractional (Product a) where
    fromRational r = Product (fromRational r)
    (Product x) / (Product y) = Product $ x / y

instance Fractional a => IntMultipliable (Product a) where
    -- Note that this could lead to a negative exponent error if x is 0.
    multiply n (Product x) = Product (x ^^ toInteger n)

instance IntMultipliable (BopPolynomial a) where
    multiply n (BP p) = BP $ (toInteger n *) <$> p

instance Num a => Commutative (Sum a)
instance Num a => Commutative (Product a)

instance Ord a => Monoid (BopPolynomial a) where
    mempty = BP []
    BP p0 `mappend` BP p1 = BP $ Map.filter (/= 0) $ Map.unionWith (+) p0 p1

instance Ord a => IsList (BopPolynomial a) where
    type Item (BopPolynomial a) = a
    fromList xs = BP $ Map.fromListWith (+) $ zip xs (repeat 1)
    toList (BP p) = do
        (x, n) <- Map.toList p
        genericReplicate n x

-- | Fold the polynomial.
foldP :: (IntMultipliable a, Monoid a) => BopPolynomial a -> a
foldP (BP p) = Map.foldrWithKey (\x n -> (multiply n x <>)) mempty p

-- | Number of distinct terms in the polynomial.
nrofDistinctTerms :: BopPolynomial a -> Int
nrofDistinctTerms = Map.size . asMap

-- | /O(n)/. The distinct terms of a polynomial., each term occurs only once in
-- the list.
--
distinctTerms :: BopPolynomial a -> [a]
distinctTerms = Map.keys . asMap

-- | /O(n)/. Convert the polynomial to a list of term\/multiplier pairs.
toMultiplierList :: BopPolynomial a -> [(a, Integer)]
toMultiplierList = toDistinctAscMultiplierList

-- | /O(n)/. Convert the polynomial to a distinct ascending list of term\/multiplier
-- pairs.
toDistinctAscMultiplierList :: BopPolynomial a -> [(a, Integer)]
toDistinctAscMultiplierList = Map.toAscList . asMap

-- | /O(n*log n)/. Create a polynomial from a list of term\/multiplier pairs.
fromMultiplierList :: Ord a => [(a, Integer)] -> BopPolynomial a
fromMultiplierList = BP . Map.filter (0/=) . Map.fromListWith (+)

-- | /O(n)/. Build a polynomial from an ascending list of term\/multiplier
-- pairs where each term appears only once. /The precondition (input list is
-- strictly ascending) is not checked./
fromDistinctAscMultiplierList :: [(a, Integer)] -> BopPolynomial a
fromDistinctAscMultiplierList = BP . Map.filter (0/=) . Map.fromDistinctAscList

-- | Append a term to the polynomial.
--
-- > append 2 [1, 2, 3]
--
-- should be equivalent to the polynomial
--
-- > [1, 2, 3, 2]
append :: Ord a => a -> BopPolynomial a -> BopPolynomial a
append = addNTimes 1

-- | Remove a term from the polynomial.
--
-- > remove 2 [1, 2, 3]
--
-- should be equivalent to the polynomial
--
-- > [1, 3]
remove :: Ord a => a -> BopPolynomial a -> BopPolynomial a
remove = addNTimes (-1)

-- | Add the term `x` `n` times. If `n` is negative the term will be removed
-- `n` times.
addNTimes :: Ord a => Integer -> a -> BopPolynomial a -> BopPolynomial a
addNTimes 0 _ s = s                                    -- invariant: no term with multiplier 0 is stored.
addNTimes m x s = (BP . Map.alter increment x . asMap) s
    where
        increment :: Maybe Integer -> Maybe Integer
        increment Nothing  = Just m
        increment (Just n) | n == -m = Nothing           -- Terms with multiplier zero are removed
        increment (Just n) = Just (n+m)

-- | /O(n)/. Partition the polynomial into two polynomial, one with all
-- elements that satisfy the predicate and one with all elements that don't
-- satisfy the predicate.
partition :: (a -> Bool) -> BopPolynomial a -> (BopPolynomial a,BopPolynomial a)
partition p = (BP *** BP) . Map.partitionWithKey (\k _ -> p k) . asMap

-- | /O(n)/. Fold over the terms of the polynomial with their multipliers.
foldMultiplier :: (a -> Integer -> b -> b) -> b -> BopPolynomial a -> b
foldMultiplier f z = Map.foldrWithKey f z . asMap

