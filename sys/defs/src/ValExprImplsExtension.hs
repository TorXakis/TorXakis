{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprImplsExtension
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (See LICENSE at root directory of this repository)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines extension of functions on and constructors of value expressions.
--
-----------------------------------------------------------------------------
module ValExprImplsExtension
( -- * Derived Boolean operators
  -- ** Or (\/)
  cstrOr
  -- ** Exclusive or (\|/)
, cstrXor
  -- ** Implies (=>)
, cstrImplies
  -- * Derived Integer operators: 
  -- ** Unary Plus
, cstrUnaryPlus
  -- ** Plus = Sum of two terms
, cstrPlus
  -- ** Minus
, cstrMinus
  -- ** Times = Product of two terms
, cstrTimes
  -- ** Absolute value
, cstrAbs
  -- * Derived Integer comparisons
  -- ** Less than (<)
, cstrLT
  -- ** Less Equal (<=)
, cstrLE
  -- ** Greater Equal (>=)
, cstrGE
  -- ** Greater Than (>)
, cstrGT
)
where

import qualified Data.Set as Set

import           Product
import           Sum
import           ValExprDefs
import           ValExprImpls


-- | Apply operator Or (\\\/) on the provided set of value expressions.
-- Preconditions are /not/ checked.
cstrOr :: Ord v => Set.Set (ValExpr v) -> ValExpr v
-- a \/ b == not (not a /\ not b)
cstrOr = cstrNot . cstrAnd . Set.map cstrNot

-- | Apply operator Xor (\\\|/) on the provided set of value expressions.
-- Preconditions are /not/ checked.
cstrXor :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
cstrXor a b = cstrOr (Set.fromList [arg0, arg1])
  where arg0 = cstrAnd (Set.fromList [a, cstrNot b])
        arg1 = cstrAnd (Set.fromList [cstrNot a, b])

-- | Apply operator Implies (=>) on the provided value expressions.
-- Preconditions are /not/ checked.
cstrImplies :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
-- a => b == not a \/ b == not (a /\ not b)
cstrImplies a b = (cstrNot . cstrAnd) (Set.insert a (Set.singleton (cstrNot b)))

-- | Apply unary operator Plus on the provided value expression.
-- Preconditions are /not/ checked.
cstrUnaryPlus :: ValExpr v -> ValExpr v
cstrUnaryPlus = id

-- | Apply operator Add on the provided value expressions.
cstrPlus :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
cstrPlus a b = cstrSum (Sum.fromList [a,b])

-- | Apply operator Minus on the provided value expressions.
cstrMinus :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
cstrMinus a b = cstrSum (Sum.fromMultiplierList [(a,1),(b,-1)])

-- | Apply operator Times on the provided value expressions.
cstrTimes :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
cstrTimes a b = cstrProduct (Product.fromList [a,b])

-- | Apply operator Abs on the provided value expression.
cstrAbs :: Ord v => ValExpr v -> ValExpr v
cstrAbs a = cstrITE (cstrGEZ a) a (cstrUnaryMinus a)

-- | Apply operator LT (<) on the provided value expression.
-- Preconditions are /not/ checked.
cstrLT :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
-- a < b <==> a - b < 0 <==> Not ( a - b >= 0 )                                                             
cstrLT ve1 ve2 = cstrNot (cstrGEZ (cstrSum (Sum.fromMultiplierList [(ve1,1),(ve2,-1)])))

-- | Apply operator GT (>) on the provided value expression.
-- Preconditions are /not/ checked.
cstrGT :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
-- a > b <==> 0 > b - a <==> Not ( 0 <= b - a )                                                             
cstrGT ve1 ve2 = cstrNot (cstrGEZ (cstrSum (Sum.fromMultiplierList [(ve1,-1),(ve2,1)])))

-- | Apply operator LE (<=) on the provided value expression.
-- Preconditions are /not/ checked.
cstrLE :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
-- a <= b <==> 0 <= b - a
cstrLE ve1 ve2 = cstrGEZ (cstrSum (Sum.fromMultiplierList [(ve1,-1),(ve2,1)]))

-- | Apply operator GE (>=) on the provided value expression.
-- Preconditions are /not/ checked.
cstrGE :: Ord v => ValExpr v -> ValExpr v -> ValExpr v
-- a >= b <==> a - b >= 0
cstrGE ve1 ve2 = cstrGEZ (cstrSum (Sum.fromMultiplierList [(ve1,1),(ve2,-1)]))
