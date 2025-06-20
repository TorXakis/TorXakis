{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Product
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation for a symbolic product.
--
-- Note: Integer division is not associative, so negative occurrences should be
-- not be used for Integers.
--
-- inspiration taken from
-- https://hackage.haskell.org/package/multiset-0.3.3/docs/src/Data-MultiSet.html
--
-- We take 0^0 == 1
--
-- In the complexity of functions /n/ refers to the number of distinct terms,
-- /t/ is the total number of terms.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Product  (
    -- * Product type
      FreeProduct
    , ProductTerm (..)

    -- * Filter
    , fraction

    -- * Product of Term and Products
    , multiply
    , divide
    , product
    , products

    -- * Power
    , power
) where

import           Control.Arrow   ((***))
import           Control.DeepSeq
import           Data.Data
import           Data.Foldable   hiding (product)
import qualified Data.Map.Strict as Map
import           Data.Monoid     hiding ((<>))
import           Data.Semigroup
import           GHC.Generics    (Generic)
import           Prelude         hiding (product)

import           FreeMonoidX     (FreeMonoidX (..), IntMultipliable,
                                  TermWrapper, (<.>))
import qualified FreeMonoidX     as FMX

import           Id
{--------------------------------------------------------------------
  The data types
--------------------------------------------------------------------}
-- |
-- `FreeProduct` represents a symbolic product of terms of the type parameter `a`.
-- The same term can occur multiple times.
type FreeProduct a = FreeMonoidX (ProductTerm a)

-- | Terms of a free-monoids of the form:
--
-- > a0 <> a1 <> ... <> an-1
--
-- where `<>` will be interpreted as the arithmetic multiplication of terms:
--
-- > a0 * a1 * ... * an-1
--
newtype ProductTerm a = ProductTerm { factor :: a }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Functor, Data)

instance (Resettable a) => Resettable (ProductTerm a)

instance Applicative ProductTerm where
    pure = ProductTerm
    fa <*> a = ProductTerm $ factor fa (factor a)

instance Num a => Semigroup (ProductTerm a) where
    pt0 <> pt1 = pure (*) <*> pt0 <*> pt1

instance Num a => Monoid (ProductTerm a) where
    mempty = pure 1

instance TermWrapper ProductTerm where
    wrap = ProductTerm
    unwrap = factor

instance Integral a => IntMultipliable (ProductTerm a) where
    n <.> pt = (^ toInteger n) <$> pt

{--------------------------------------------------------------------
  Products and multiplications
--------------------------------------------------------------------}
-- | /O(log n)/. Multiply a product with a term.
multiply :: Ord a => a -> FreeProduct a -> FreeProduct a
multiply = FMX.append . ProductTerm

-- | /O(log n)/. Divide a product by a term.
divide :: Ord a => a -> FreeProduct a -> FreeProduct a
divide = FMX.remove . ProductTerm

-- | The product of a list of products.
products :: Ord a => [FreeProduct a] -> FreeProduct a
products = fold

-- | /O(n+m)/. The product of two products.
--
product :: Ord a => FreeProduct a -> FreeProduct a -> FreeProduct a
product = (<>)

-- | /O(n)/. Take the product /ms/ to the power with the constant /x/.
power :: Ord a => Integer -> FreeProduct a -> FreeProduct a
power = (<.>)


{--------------------------------------------------------------------
  Partition
--------------------------------------------------------------------}
-- | /O(n)/. Partition the product into the dividend and divisor.
fraction :: Ord a => FreeProduct a -> (FreeProduct a, FreeProduct a)
fraction =
    (FMX *** (power (-1). FMX) ) . Map.partition (>= 0) . asMap
