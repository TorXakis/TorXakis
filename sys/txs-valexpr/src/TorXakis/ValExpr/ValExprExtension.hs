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
