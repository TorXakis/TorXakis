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
-- > a0 <+> a1 <+> ... <+> an-1
--
-- `ai \in A`, and <+> is a commutative (binary) operation, and (A, <+>) is a
-- Monoid.
-----------------------------------------------------------------------------

module BopPolynomial
  ( -- * Binary Operation Polynomial Type.
    BopPolynomial
    -- * Query.
  , nrofTerms
    -- * Manipulation of the polynomial.
  , append
  , remove
  , addNTimes
    -- * TODO: classify these operations.
  , asMap
  , (<+>)
  , multiply
  , Multiply
  , foldP
  -- * Product wrapper, and operations.
  , MyProduct(..)
  )
where

import           Data.AEq
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     hiding (Product (..))
import           GHC.Exts

newtype BopPolynomial a = BP { asMap :: Map a Int }

instance Show a => Show (BopPolynomial a) where
    show (BP p) = show (Map.assocs p)

-- | Types with a commutative operator.
class Commutative a where
    (<+>) :: a -> a -> a

-- | Types that can be multiplied by a negative constant.
class (Num a, Monoid a) => Multiply a where
    -- | `multiply n x` multiplies `x` `n` times.
    multiply :: Integral n
             => n -- ^ Multiplication factor.
             -> a -- ^ Element to multiply.
             -> a

instance Integral a => Multiply (Sum a) where
    multiply n (Sum x) = Sum (fromInteger $ toInteger x * toInteger n)

newtype MyProduct a = MyProduct {getProduct :: a} deriving (Show, Functor, Num, Ord)

instance AEq a => Eq (MyProduct a) where
    (MyProduct x) == (MyProduct y) = x ~== y

instance Num a => Monoid (MyProduct a) where
    mempty = MyProduct 1
    (MyProduct x) `mappend` (MyProduct y) = MyProduct $ x * y

instance Fractional a => Fractional (MyProduct a) where
    fromRational r = MyProduct (fromRational r)
    (MyProduct x) / (MyProduct y) = MyProduct $ x / y

instance Fractional a => Multiply (MyProduct a) where
    -- Note that this could lead to a negative exponent error if x is 0.
    multiply n (MyProduct x) = MyProduct (x ^^ toInteger n)

instance Num a => Commutative (Sum a) where
    (Sum x) <+> (Sum y) = Sum (x + y)

instance
    ( Monoid a
    , Commutative a
    , Ord a
    , Multiply a
    ) => Monoid (BopPolynomial a) where
    mempty = BP []
    BP p0 `mappend` BP p1 = BP $ Map.filter (/= 0) $ Map.unionWith (+) p0 p1

instance Ord a => IsList (BopPolynomial a) where
    type Item (BopPolynomial a) = a
    fromList xs = BP $ Map.fromListWith (+) $ zip xs (repeat 1)
    toList (BP p) = do
        (x, n) <- Map.toList p
        replicate n x

-- | Fold the polynomial.
foldP :: Multiply a => BopPolynomial a -> a
foldP (BP p) = Map.foldrWithKey (\x n -> (multiply n x <>)) mempty p

-- | Number of distinct terms in the polynomial.
nrofTerms :: BopPolynomial a -> Int
nrofTerms = Map.size . asMap

-- | Append a term to the polynomial.
--
-- > append 2 [1, 2, 3]
--
-- should be equivalent to the polynomial
--
-- > [1, 2, 3, 2]
append :: Ord a => a -> BopPolynomial a -> BopPolynomial a
append = undefined

-- | Remove a term from the polynomial.
--
-- > remove 2 [1, 2, 3]
--
-- should be equivalent to the polynomial
--
-- > [1, 3]
remove :: Ord a => a -> BopPolynomial a -> BopPolynomial a
remove = undefined

-- | Add the term `x` `n` times. If `n` is negative the term will be removed.
addNTimes :: Ord a => a -> Int -> BopPolynomial a -> BopPolynomial a
addNTimes _ 0 s = s                                    -- invariant: no term with multiplier 0 is stored.
addNTimes x m s = (BP . Map.alter increment x . asMap) s
    where
        increment :: Maybe Int -> Maybe Int
        increment Nothing  = Just m
        increment (Just n) | n == -m = Nothing           -- Terms with multiplier zero are removed
        increment (Just n) = Just (n+m)
