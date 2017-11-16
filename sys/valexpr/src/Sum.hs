{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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

    -- * Sum of Term and Sums
    , add
    , subtract
    , sum
    , sums

    -- * Multiply
    , multiply
    ) where

import           Prelude         hiding (subtract, sum)

import           Control.DeepSeq
import           Data.Foldable   hiding (sum)
import           Data.Monoid     ((<>))
import           FreeMonoidX     (FreeMonoidX, IntMultipliable, TermWrapper,
                                  (<.>))
import qualified FreeMonoidX     as FMX
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

instance TermWrapper SumTerm where
    wrap = SumTerm
    unwrap = summand

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