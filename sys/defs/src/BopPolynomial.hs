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
--  Binary operation polynomials: a symbolic representation of polynomials of
--  the form:
--
-- > a0 <+> a1 <+> ... <+> an-1
--
-- `ai \in A`, and <+> is a commutative (binary) operation, and (A, <+>) is a
-- Monoid.
-----------------------------------------------------------------------------

module BopPolynomial
  ( asMap
  , (<+>)
  , multiply
  , Multiply
  , foldP
  , BopPolynomial
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           GHC.Exts

newtype BopPolynomial a = BP { asMap :: Map a Int }

instance Show a => Show (BopPolynomial a) where
    show (BP p) = show (Map.assocs p)

-- | Types with a commutative operator.
class Commutative a where
    (<+>) :: a -> a -> a

-- | Types that can be multiplied by a non-negative constant.
class (Num a, Monoid a) => Multiply a where
    -- | `multiply n x` multiplies `x` `n` times.
    multiply :: Integral n
             => n -- ^ Multiplication factor.
             -> a -- ^ Element to multiply.
             -> a

instance Integral a => Multiply (Sum a) where
    multiply n (Sum x) = Sum (fromInteger $ toInteger x * toInteger n)

instance Integral a => Multiply (Product a) where
    multiply n (Product x) = Product (fromInteger $ toInteger x ^ toInteger n)

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

-- | Fold the polynomial.
foldP :: Multiply a => BopPolynomial a -> a
foldP (BP p) = Map.foldrWithKey (\x n -> (multiply n x <>)) mempty p

instance Ord a => IsList (BopPolynomial a) where
    type Item (BopPolynomial a) = a
    fromList xs = BP $ Map.fromListWith (+) $ zip xs (repeat 1)
    toList (BP p) = do
        (x, n) <- Map.toList p
        replicate n x
