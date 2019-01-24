{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprConstructor
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Smart constructors for Value Expressions.
-----------------------------------------------------------------------------
module TorXakis.ValExpr.ValExprExtension
( -- ** Convenience Constructors to create Value Expression
  -- *** Boolean Operators to create Value Expressions
  -- **** Or (\\/)
  mkOr
  -- **** eXclusive Or (\\|/)
, mkXor
  -- **** Implies (=>)
, mkImplies
  -- *** Integer Operators to create Value Expressions
  -- **** Unary Plus
, mkUnaryPlus
  -- **** Plus
, mkPlus
  -- **** Minus
, mkMinus
  -- **** Times
, mkTimes
  -- **** Absolute value
, mkAbs
  -- *** Derived Integer comparisons
  -- **** Less than (<)
, mkLT
  -- **** Less Equal (<=)
, mkLE
  -- **** Greater Equal (>=)
, mkGE
  -- **** Greater Than (>)
, mkGT
)
where
import           Data.Either
import qualified Data.Text       as T

import           TorXakis.Error
import           TorXakis.Sort
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.VarContext

-- | Apply operator Or (\\/) on the provided list of value expressions.
mkOr :: VarContext c => c -> [ValExpression] -> Either MinError ValExpression
-- a \/ b <==> not (not a /\ not b)
mkOr ctx s | all (\e -> SortBool == getSort ctx e) s = case partitionEithers (fmap (mkNot ctx) s) of
                                                           ([] , ns) -> mkAnd ctx ns >>= mkNot ctx
                                                           (es, _)   -> error ("mkNot on booleans should succeed in Or. However: " ++ show es)
mkOr _ _                                             = Left $ MinError (T.pack "Not all value expressions in set are of expected sort Bool")

-- | Apply operator Xor (\\|/) on the provided set of value expressions.
mkXor :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkXor ctx a _ | SortBool /= getSort ctx a = Left $ MinError (T.pack ("First argument of Xor is not of expected sort Bool but " ++ show (getSort ctx a)))
mkXor ctx _ b | SortBool /= getSort ctx b = Left $ MinError (T.pack ("Second argument of Xor is not of expected sort Bool but " ++ show (getSort ctx b)))
-- a xor b <==> (a /\ not b) \/ (not a /\ b)
mkXor ctx a b                           = mkNot ctx a >>= (\na ->
                                          mkNot ctx b >>= (\nb -> 
                                                mkAnd ctx [a,nb] >>= (\a1 ->
                                                mkAnd ctx [na,b] >>= (\a2 ->
                                                        mkOr ctx [a1,a2] )) ))

-- | Apply operator Implies (=>) on the provided value expressions.
mkImplies :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkImplies ctx a _ | SortBool /= getSort ctx a = Left $ MinError (T.pack ("First argument of Implies is not of expected sort Bool but " ++ show (getSort ctx a)))
mkImplies ctx _ b | SortBool /= getSort ctx b = Left $ MinError (T.pack ("Second argument of Implies is not of expected sort Bool but " ++ show (getSort ctx b)))
-- a => b <==> not a \/ b <==> not (a /\ not b)
mkImplies ctx a b                         = mkNot ctx b >>= (\nb -> mkAnd ctx [a,nb] >>= mkNot ctx)

-- | Apply unary operator Plus on the provided value expression.
mkUnaryPlus :: VarContext c => c -> ValExpression -> Either MinError ValExpression
mkUnaryPlus ctx v | getSort ctx v == SortInt = Right v
mkUnaryPlus ctx v                            = Left $ MinError (T.pack ("Unary Plus argument not of expected Sort Int but " ++ show (getSort ctx v)))

-- | Apply operator Plus on the provided value expressions.
-- Plus is the 'mkSum' of two Value Expressions.
mkPlus :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkPlus ctx a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of Plus is not of expected sort Int but " ++ show (getSort ctx a)))
mkPlus ctx _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of Plus is not of expected sort Int but " ++ show (getSort ctx b)))
mkPlus ctx a b                            = mkSum ctx [a,b]

-- | Apply operator Minus on the provided value expressions.
-- Minus is the difference of two Value Expressions.
mkMinus :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkMinus ctx a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of Minus is not of expected sort Int but " ++ show (getSort ctx a)))
mkMinus ctx _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of Minus is not of expected sort Int but " ++ show (getSort ctx b)))
mkMinus ctx a b                            = mkUnaryMinus ctx b >>= (\nb -> mkSum ctx [a,nb])

-- | Apply operator Times on the provided value expressions.
-- Times is the 'mkProduct' of two Value Expressions.
mkTimes :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkTimes ctx a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of Times is not of expected sort Int but " ++ show (getSort ctx a)))
mkTimes ctx _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of Times is not of expected sort Int but " ++ show (getSort ctx b)))
mkTimes ctx a b                            = mkProduct ctx [a,b]

-- | Apply operator Abs on the provided value expression.
mkAbs :: VarContext c => c -> ValExpression -> Either MinError ValExpression
mkAbs ctx a | SortInt /= getSort ctx a = Left $ MinError (T.pack ("Argument of Abs is not of expected sort Int but " ++ show (getSort ctx a)))
-- abs (x) <==> IF (x >=0) THEN x ELSE -x
mkAbs ctx a                            = mkUnaryMinus ctx a >>= (\na -> mkGEZ ctx a >>= (\c -> mkITE ctx c a na))

-- | Apply operator Less Then (<) on the provided value expressions.
mkLT :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkLT ctx a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of LT is not of expected sort Int but " ++ show (getSort ctx a)))
mkLT ctx _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of LT is not of expected sort Int but " ++ show (getSort ctx b)))
-- a < b <==> a - b < 0 <==> Not ( a - b >= 0 )
mkLT ctx a b                            = mkMinus ctx a b >>= mkGEZ ctx >>= mkNot ctx

-- | Apply operator Greater Then (>) on the provided value expressions.
mkGT :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkGT ctx a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of GT is not of expected sort Int but " ++ show (getSort ctx a)))
mkGT ctx _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of GT is not of expected sort Int but " ++ show (getSort ctx b)))
-- a > b <==> 0 > b - a <==> Not ( 0 <= b - a )
mkGT ctx a b                            = mkMinus ctx b a >>= mkGEZ ctx >>= mkNot ctx

-- | Apply operator Less Equal (<=) on the provided value expressions.
mkLE :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkLE ctx   a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of LE is not of expected sort Int but " ++ show (getSort ctx a)))
mkLE ctx   _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of LE is not of expected sort Int but " ++ show (getSort ctx b)))
-- a <= b <==> 0 <= b - a
mkLE ctx a b                        = mkMinus ctx b a >>= mkGEZ ctx

-- | Apply operator Less Equal (>=) on the provided value expressions.
mkGE :: VarContext c => c -> ValExpression -> ValExpression -> Either MinError ValExpression
mkGE ctx   a _ | SortInt /= getSort ctx a = Left $ MinError (T.pack ("First argument of GE is not of expected sort Int but " ++ show (getSort ctx a)))
mkGE ctx   _ b | SortInt /= getSort ctx b = Left $ MinError (T.pack ("Second argument of GE is not of expected sort Int but " ++ show (getSort ctx b)))
-- a >= b <==> a - b >= 0
mkGE ctx a b                        = mkMinus ctx a b >>= mkGEZ ctx
