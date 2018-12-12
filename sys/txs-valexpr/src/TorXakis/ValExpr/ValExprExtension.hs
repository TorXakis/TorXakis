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

-- | Apply operator Or (\\/) on the provided list of value expressions.
mkOr :: ValExprContext c v => c v -> [ValExpression v] -> Either MinError (ValExpression v)
-- a \/ b <==> not (not a /\ not b)
mkOr c s | all (\e -> SortBool == getSort e) s = case partitionEithers (fmap (mkNot c) s) of
                                                      ([] , ns) -> mkAnd c ns >>= mkNot c
                                                      (es, _)   -> error ("mkNot on booleans should succeed in Or. However: " ++ show es)
mkOr _ _                                       = Left $ MinError (T.pack "Not all value expressions in set are of expected sort Bool")

-- | Apply operator Xor (\\|/) on the provided set of value expressions.
mkXor :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkXor _ a _ | SortBool /= getSort a = Left $ MinError (T.pack ("First argument of Xor is not of expected sort Bool but " ++ show (getSort a)))
mkXor _ _ b | SortBool /= getSort b = Left $ MinError (T.pack ("Second argument of Xor is not of expected sort Bool but " ++ show (getSort b)))
-- a xor b <==> (a /\ not b) \/ (not a /\ b)
mkXor c a b                         = mkNot c a >>= (\na ->
                                      mkNot c b >>= (\nb -> 
                                            mkAnd c [a,nb] >>= (\a1 ->
                                            mkAnd c [na,b] >>= (\a2 ->
                                                    mkOr c [a1,a2] )) ))

-- | Apply operator Implies (=>) on the provided value expressions.
mkImplies :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkImplies _ a _ | SortBool /= getSort a = Left $ MinError (T.pack ("First argument of Implies is not of expected sort Bool but " ++ show (getSort a)))
mkImplies _ _ b | SortBool /= getSort b = Left $ MinError (T.pack ("Second argument of Implies is not of expected sort Bool but " ++ show (getSort b)))
-- a => b <==> not a \/ b <==> not (a /\ not b)
mkImplies c a b                         = mkNot c b >>= (\nb -> mkAnd c [a,nb] >>= mkNot c)

-- | Apply unary operator Plus on the provided value expression.
mkUnaryPlus :: ValExprContext c v => c v -> ValExpression v -> Either MinError (ValExpression v)
mkUnaryPlus _ v | getSort v == SortInt = Right v
mkUnaryPlus _ v                        = Left $ MinError (T.pack ("Unary Plus argument not of expected Sort Int but " ++ show (getSort v)))

-- | Apply operator Plus on the provided value expressions.
-- Plus is the 'mkSum' of two Value Expressions.
mkPlus :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkPlus _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of Plus is not of expected sort Int but " ++ show (getSort a)))
mkPlus _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of Plus is not of expected sort Int but " ++ show (getSort b)))
mkPlus ctx a b                        = mkSum ctx [a,b]

-- | Apply operator Minus on the provided value expressions.
-- Minus is the difference of two Value Expressions.
mkMinus :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkMinus _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of Minus is not of expected sort Int but " ++ show (getSort a)))
mkMinus _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of Minus is not of expected sort Int but " ++ show (getSort b)))
mkMinus ctx a b                        = mkUnaryMinus ctx b >>= (\nb -> mkSum ctx [a,nb])

-- | Apply operator Times on the provided value expressions.
-- Times is the 'mkProduct' of two Value Expressions.
mkTimes :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkTimes _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of Times is not of expected sort Int but " ++ show (getSort a)))
mkTimes _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of Times is not of expected sort Int but " ++ show (getSort b)))
mkTimes ctx a b                        = mkProduct ctx [a,b]

-- | Apply operator Abs on the provided value expression.
mkAbs :: ValExprContext c v => c v -> ValExpression v -> Either MinError (ValExpression v)
mkAbs _   a | SortInt /= getSort a = Left $ MinError (T.pack ("Argument of Abs is not of expected sort Int but " ++ show (getSort a)))
-- abs (x) <==> IF (x >=0) THEN x ELSE -x
mkAbs ctx a                        = mkUnaryMinus ctx a >>= (\na -> mkGEZ ctx a >>= (\c -> mkITE ctx c a na))

-- | Apply operator Less Then (<) on the provided value expressions.
mkLT :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkLT _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of LT is not of expected sort Int but " ++ show (getSort a)))
mkLT _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of LT is not of expected sort Int but " ++ show (getSort b)))
-- a < b <==> a - b < 0 <==> Not ( a - b >= 0 )
mkLT ctx a b                        = mkMinus ctx a b >>= mkGEZ ctx >>= mkNot ctx

-- | Apply operator Greater Then (>) on the provided value expressions.
mkGT :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkGT _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of GT is not of expected sort Int but " ++ show (getSort a)))
mkGT _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of GT is not of expected sort Int but " ++ show (getSort b)))
-- a > b <==> 0 > b - a <==> Not ( 0 <= b - a )
mkGT ctx a b                        = mkMinus ctx b a >>= mkGEZ ctx >>= mkNot ctx

-- | Apply operator Less Equal (<=) on the provided value expressions.
mkLE :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkLE _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of LE is not of expected sort Int but " ++ show (getSort a)))
mkLE _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of LE is not of expected sort Int but " ++ show (getSort b)))
-- a <= b <==> 0 <= b - a
mkLE ctx a b                        = mkMinus ctx b a >>= mkGEZ ctx


-- | Apply operator Less Equal (>=) on the provided value expressions.
mkGE :: ValExprContext c v => c v -> ValExpression v -> ValExpression v -> Either MinError (ValExpression v)
mkGE _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of GE is not of expected sort Int but " ++ show (getSort a)))
mkGE _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of GE is not of expected sort Int but " ++ show (getSort b)))
-- a >= b <==> a - b >= 0
mkGE ctx a b                        = mkMinus ctx a b >>= mkGEZ ctx
