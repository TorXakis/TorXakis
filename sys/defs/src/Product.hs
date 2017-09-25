{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Product
-- Copyright   :  (c) TNO and Radboud University
-- License     :  Closed-style (see the file license.txt)
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
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Product  (
    -- * Product type
      FreeProduct
    , ProductTerm (..)

    -- * Query
    , distinctTerms     -- exposed for performance reasons
                        -- checking properties for all distinct terms is faster than for all terms

    -- * Filter
    , partition
    , fraction

    -- * Product of Term and Products
    , multiply
    , divide
    , product
    , products

    -- * Power
    , power

    -- * Constructors and conversion
    -- ** List
    , fromList

    -- ** Power lists
    , toPowerList
    , toDistinctAscPowerList
    , fromPowerList
    , fromDistinctAscPowerList
) where

import           Prelude         hiding (product)

import           Control.Arrow   (first, second, (***))
import           Control.DeepSeq
import           Data.Foldable   hiding (product)
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified GHC.Exts        as Exts
import           GHC.Generics    (Generic)

import           FreeMonoidX     (FreeMonoidX (..), IntMultipliable, (<.>))
import qualified FreeMonoidX     as FMX
{--------------------------------------------------------------------
  The data type
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
    deriving (Eq, Ord, Num, Read, Show, Generic, NFData, Functor)

instance Applicative ProductTerm where
    pure = ProductTerm
    fa <*> a = ProductTerm $ factor fa (factor a)

instance Num a => Monoid (ProductTerm a) where
    mempty = pure 0
    pt0 `mappend` pt1 = pure (*) <*> pt0 <*> pt1

instance Integral a => IntMultipliable (ProductTerm a) where
    -- Instances of `IntMultipliable` for `PPproduct` are only defined for
    -- fractional numbers. We cannot define this for `Int` (or `Integer`) since
    -- the multiplicative inverse is not defined for them.
    --
    -- TODO: the right constraint should be:
    --
    -- > Fractional a => IntMultipliable (ProductTerm a)
    --
    -- However in the current state of we don't have fractional numbers or
    -- double.

    -- See https://wiki.haskell.org/Power_function
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
-- | /O(n)/. Partition the product into two products, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
partition :: (a -> Bool) -> FreeProduct a -> (FreeProduct a, FreeProduct a)
partition p = FMX.partition (p . factor)


-- | /O(n)/. Partition the product into the dividend and divisor.
fraction :: Ord a => FreeProduct a -> (FreeProduct a, FreeProduct a)
fraction =
    (FMX *** (power (-1). FMX) ) . Map.partition (>= 0) . asMap

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n)/. The distinct terms of a product,
-- each term occurs only once in the list.
--
-- > distinctTerms = map fst . toOccurList
distinctTerms :: FreeProduct a -> [a]
distinctTerms = (factor <$>). FMX.distinctTerms

-- | /O(t*log t)/. Create a product from a list of terms.
fromList :: Ord a => [a] -> FreeProduct a
fromList = Exts.fromList . (ProductTerm <$>)

{--------------------------------------------------------------------
  Multiplier lists
--------------------------------------------------------------------}

-- | /O(n)/. Convert the product to a list of term\/power pairs.
toPowerList :: FreeProduct a -> [(a, Integer)]
toPowerList = (first factor <$>) . FMX.toDistinctAscMultiplierList

-- | /O(n)/. Convert the product to a distinct ascending list of term\/power pairs.
toDistinctAscPowerList :: FreeProduct a -> [(a, Integer)]
toDistinctAscPowerList = (first factor <$>) . FMX.toDistinctAscMultiplierList

-- | /O(n*log n)/. Create a product from a list of term\/power pairs.
fromPowerList :: Ord a => [(a, Integer)] -> FreeProduct a
fromPowerList = FMX.fromMultiplierList . (first ProductTerm <$>)

-- | /O(n)/. Build a product from an ascending list of term\/power pairs where
-- each term appears only once.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscPowerList :: [(a, Integer)] -> FreeProduct a
fromDistinctAscPowerList =
    FMX.fromDistinctAscMultiplierList . (first ProductTerm <$>)
