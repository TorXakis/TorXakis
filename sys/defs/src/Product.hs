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
-- Note: Integer division is not associative, so negative occurances should be not be used for Integers.
--
-- inspiration taken from
-- https://hackage.haskell.org/package/multiset-0.3.3/docs/src/Data-MultiSet.html
--
-- We take 0^0 == 1
--
-- In the complexity of functions /n/ refers to the number of distinct terms,
-- /t/ is the total number of terms.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Product  ( 
    -- * Product type
      Product

    -- * Query
    , nrofTerms
    , distinctTerms     -- exposed for performance reasons
                        -- checking properties for all distinct terms is faster than for all terms

    -- * Filter
    , partition
    , fraction
    
    -- * Fold
    , foldPower
    
    -- * Product of Term and Products
    , multiply
    , divide
    , multiplyPower
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

import Prelude hiding (product)

import Control.Arrow ((***))
import Control.DeepSeq
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
{--------------------------------------------------------------------
  The data type
--------------------------------------------------------------------}
-- | Product is a parameterized data type.
-- Product represents a symbolic product of terms of the type parameter @a@.
-- The same term can occur multiple times.
newtype Product a = Product { unProduct :: Map.Map a Integer }
    deriving (Eq, Ord, Read, Show, Generic, NFData)
{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. The number of distinct terms in the product.
nrofTerms :: Product a -> Int
nrofTerms = Map.size . unProduct

{--------------------------------------------------------------------
  Products and multiplications
--------------------------------------------------------------------}
-- | /O(log n)/. Multiply a product with a term.
multiply :: Ord a => a -> Product a -> Product a
multiply t = multiplyPower t 1

-- | /O(log n)/. Divide a product by a term.
divide :: Ord a => a -> Product a -> Product a
divide t = multiplyPower t (-1)

-- | /O(log n)/. Multiply a term to the given power with a product.
multiplyPower :: Ord a => a -> Integer -> Product a -> Product a
multiplyPower _ 0 p = p                                 -- invariant: no term with power 0 is stored.
multiplyPower x m p = (Product . Map.alter increment x . unProduct) p
    where
        increment :: Maybe Integer -> Maybe Integer
        increment Nothing            = Just m
        increment (Just n) | n == -m = Nothing          -- Terms with power zero are removed
        increment (Just n)           = Just (n+m)

-- | The product of a list of products.
products :: Ord a => [Product a] -> Product a
products = foldlStrict product (Product Map.empty)

-- | /O(n+m)/. The product of two products.
--
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigproduct `product` smallproduct).
product :: Ord a => Product a -> Product a -> Product a
product (Product m1) (Product m2) = Product $ Map.filter (0/=) $ Map.unionWith (+) m1 m2   -- Terms with power zero are removed

-- | /O(n)/. Take the product /ms/ to the power with the constant /x/.
power :: Integer -> Product a -> Product a
power 0 _ = Product Map.empty
power n s = (Product . Map.map (n *) . unProduct) s

{--------------------------------------------------------------------
  Partition
--------------------------------------------------------------------}
-- | /O(n)/. Partition the product into two products, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
partition :: (a -> Bool) -> Product a -> (Product a, Product a)
partition p = (Product *** Product) . Map.partitionWithKey (\k _ -> p k) . unProduct

{--------------------------------------------------------------------
  Fraction
--------------------------------------------------------------------}
-- | /O(n)/. Partition the product into the dividend and divisor.
fraction :: Product a -> (Product a, Product a)
fraction = (Product *** (power (-1). Product) ) . Map.partition (>= 0) . unProduct

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

-- | /O(n)/. Fold over the terms of a product with their multipliers.
foldPower :: (a -> Integer -> b -> b) -> b -> Product a -> b
foldPower f z = Map.foldrWithKey f z . unProduct

{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}

-- | /O(n)/. The distinct terms of a product, 
-- each term occurs only once in the list.
--
-- > distinctTerms = map fst . toOccurList
distinctTerms :: Product a -> [a]
distinctTerms = Map.keys . unProduct

-- | /O(t*log t)/. Create a product from a list of terms.
fromList :: Ord a => [a] -> Product a 
fromList xs = fromPowerList $ zip xs (repeat 1)

{--------------------------------------------------------------------
  Multiplier lists 
--------------------------------------------------------------------}

-- | /O(n)/. Convert the product to a list of term\/power pairs.
toPowerList :: Product a -> [(a, Integer)]
toPowerList = toDistinctAscPowerList

-- | /O(n)/. Convert the product to a distinct ascending list of term\/power pairs.
toDistinctAscPowerList :: Product a -> [(a, Integer)]
toDistinctAscPowerList = Map.toAscList . unProduct

-- | /O(n*log n)/. Create a product from a list of term\/power pairs.
fromPowerList :: Ord a => [(a, Integer)] -> Product a 
fromPowerList = Product . Map.filter (0/=) . Map.fromListWith (+)       -- Terms with power zero are removed

-- | /O(n)/. Build a product from an ascending list of term\/power pairs where 
-- each term appears only once.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscPowerList :: [(a, Integer)] -> Product a 
fromDistinctAscPowerList = Product . Map.filter (0/=) . Map.fromDistinctAscList

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- TODO : Use foldl' from base?
foldlStrict :: (a -> t -> a) -> a -> [t] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)