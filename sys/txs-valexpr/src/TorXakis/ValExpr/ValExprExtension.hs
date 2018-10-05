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
import qualified Data.Set        as Set
import qualified Data.Text       as T

import           TorXakis.Error
import           TorXakis.Sort
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprConstructor
import           TorXakis.ValExpr.ValExprContext

-- | Apply operator Or (\\/) on the provided set of value expressions.
mkOr :: (Ord v, ValExprContext c v) => c -> Set.Set (ValExpr v) -> Either MinError (ValExpr v)
-- a \/ b <==> not (not a /\ not b)
mkOr c s | all (\e -> SortBool == getSort e) (Set.toList s) = case partitionEithers (map (mkNot c) (Set.toList s)) of
                                                                    ([] , ns) -> case mkAnd c (Set.fromList ns) of
                                                                                    Right a -> mkNot c a
                                                                                    Left e  -> error ("mkAnd on booleans should succeed in Or. However: " ++ show e)
                                                                    (es, _)   -> error ("mkNot on booleans should succeed in Or. However: " ++ show es)
mkOr _ _                                                    = Left $ MinError (T.pack "Not all value expressions in set are of expected sort Bool")

-- | Apply operator Xor (\\|/) on the provided set of value expressions.
mkXor :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkXor _ a _ | SortBool /= getSort a = Left $ MinError (T.pack ("First argument of Xor is not of expected sort Bool but " ++ show (getSort a)))
mkXor _ _ b | SortBool /= getSort b = Left $ MinError (T.pack ("Second argument of Xor is not of expected sort Bool but " ++ show (getSort b)))
mkXor c a b                         = case mkNot c a of
                                            Left e   -> error ("mkNot on boolean a should succeed in Xor. However: " ++ show e)
                                            Right na -> case mkNot c b of
                                                            Left e   -> error ("mkNot on boolean b should succeed in Xor. However: " ++ show e)
                                                            Right nb -> case mkAnd c (Set.fromList [a,nb]) of
                                                                            Left e   -> error ("First mkAnd should succeed in Xor. However: " ++ show e)
                                                                            Right a1 -> case mkAnd c (Set.fromList [na,b]) of
                                                                                            Left e   -> error ("Second mkAnd should succeed in Xor. However: " ++ show e)
                                                                                            Right a2 -> mkOr c (Set.fromList [a1,a2])

-- | Apply operator Implies (=>) on the provided value expressions.
mkImplies :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkImplies _ a _ | SortBool /= getSort a = Left $ MinError (T.pack ("First argument of Implies is not of expected sort Bool but " ++ show (getSort a)))
mkImplies _ _ b | SortBool /= getSort b = Left $ MinError (T.pack ("Second argument of Implies is not of expected sort Bool but " ++ show (getSort b)))
-- a => b <==> not a \/ b <==> not (a /\ not b)
mkImplies c a b                         = case mkNot c b of
                                            Left e   -> error ("mkNot on boolean b should succeed in Implies. However: " ++ show e)
                                            Right nb -> case mkAnd c (Set.fromList [a,nb]) of
                                                            Left e   -> error ("mkAnd on set of booleans should succeed in Implies. However: " ++ show e)
                                                            Right as -> mkNot c as

-- | Apply unary operator Plus on the provided value expression.
mkUnaryPlus :: ValExprContext c v => c -> ValExpr v -> Either MinError (ValExpr v)
mkUnaryPlus _ v | getSort v == SortInt = Right $ v
mkUnaryPlus _ v                        = Left $ MinError (T.pack ("Unary Plus argument not of expected Sort Int but " ++ show (getSort v)))

-- | Apply operator Plus on the provided value expressions.
-- Plus is the 'mkSum' of two Value Expressions.
mkPlus :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkPlus _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of Plus is not of expected sort Int but " ++ show (getSort a)))
mkPlus _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of Plus is not of expected sort Int but " ++ show (getSort b)))
mkPlus ctx a b                        = mkSum ctx [a,b]

-- | Apply operator Minus on the provided value expressions.
-- Minus is the difference of two Value Expressions.
mkMinus :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkMinus _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of Minus is not of expected sort Int but " ++ show (getSort a)))
mkMinus _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of Minus is not of expected sort Int but " ++ show (getSort b)))
mkMinus ctx a b                        = case mkUnaryMinus ctx b of 
                                            Left e   -> error ("mkUnaryMinus on Int b should succeed in mkMinus. However: " ++ show e)
                                            Right nb -> mkSum ctx [a,nb]

-- | Apply operator Times on the provided value expressions.
-- Times is the 'mkProduct' of two Value Expressions.
mkTimes :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkTimes _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of Times is not of expected sort Int but " ++ show (getSort a)))
mkTimes _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of Times is not of expected sort Int but " ++ show (getSort b)))
mkTimes ctx a b                        = mkProduct ctx [a,b]

-- | Apply operator Abs on the provided value expression.
mkAbs :: (Ord v, ValExprContext c v) => c -> ValExpr v -> Either MinError (ValExpr v)
mkAbs _   a | SortInt /= getSort a = Left $ MinError (T.pack ("Argument of Abs is not of expected sort Int but " ++ show (getSort a)))
mkAbs ctx a                        = case mkUnaryMinus ctx a of
                                            Left e   -> error ("mkUnaryMinus on Int a should succeed in mkAbs. However: " ++ show e)
                                            Right na -> case mkGEZ ctx a of
                                                            Left e  -> error ("mkGEZ on Int a should succeed in mkAbs. However: " ++ show e)
                                                            Right c -> mkITE ctx c a na

-- | Apply operator Less Then (<) on the provided value expressions.
mkLT :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkLT _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of LT is not of expected sort Int but " ++ show (getSort a)))
mkLT _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of LT is not of expected sort Int but " ++ show (getSort b)))
-- a < b <==> a - b < 0 <==> Not ( a - b >= 0 )
mkLT ctx a b                        = case mkMinus ctx a b of 
                                            Left e  -> error ("mkMinus should succeed in mkLT. However: " ++ show e)
                                            Right s -> case mkGEZ ctx s of 
                                                            Left e  -> error ("mkGEZ should succeed in mkLT. However: " ++ show e)
                                                            Right c -> mkNot ctx c

-- | Apply operator Greater Then (>) on the provided value expressions.
mkGT :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkGT _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of GT is not of expected sort Int but " ++ show (getSort a)))
mkGT _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of GT is not of expected sort Int but " ++ show (getSort b)))
-- a > b <==> 0 > b - a <==> Not ( 0 <= b - a )
mkGT ctx a b                        = case mkMinus ctx b a of 
                                            Left e  -> error ("mkMinus should succeed in mkGT. However: " ++ show e)
                                            Right s -> case mkGEZ ctx s of 
                                                            Left e  -> error ("mkGEZ should succeed in mkGT. However: " ++ show e)
                                                            Right c -> mkNot ctx c

-- | Apply operator Less Equal (<=) on the provided value expressions.
mkLE :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkLE _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of LE is not of expected sort Int but " ++ show (getSort a)))
mkLE _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of LE is not of expected sort Int but " ++ show (getSort b)))
-- a <= b <==> 0 <= b - a
mkLE ctx a b                        = case mkMinus ctx b a of 
                                            Left e  -> error ("mkMinus should succeed in mkLE. However: " ++ show e)
                                            Right s -> mkGEZ ctx s


-- | Apply operator Less Equal (>=) on the provided value expressions.
mkGE :: (Ord v, ValExprContext c v) => c -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkGE _   a _ | SortInt /= getSort a = Left $ MinError (T.pack ("First argument of GE is not of expected sort Int but " ++ show (getSort a)))
mkGE _   _ b | SortInt /= getSort b = Left $ MinError (T.pack ("Second argument of GE is not of expected sort Int but " ++ show (getSort b)))
-- a >= b <==> a - b >= 0
mkGE ctx a b                        = case mkMinus ctx a b of 
                                            Left e  -> error ("mkMinus should succeed in mkGE. However: " ++ show e)
                                            Right s -> mkGEZ ctx s
