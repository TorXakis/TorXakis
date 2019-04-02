{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Subst
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module TorXakis.UnsafeFunc
( unsafeFunc
)
where
import           Control.Arrow          (first)
import           Data.Either
import qualified Data.HashMap           as HashMap
import           Data.List
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Data.Text              as T

import           TorXakis.ContextValExpr
import           TorXakis.Error
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.ValExpr.Unsafe
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.Var

-- TODO? More laziness?
-- e.g. depending on some parameter value, some other parameter values might be irrelevant
-- e.g. ANY/Error for not initialized variables of State Automaton translated to a ProcDef
unsafeFunc :: ValExprContext c => c -> RefByFuncSignature -> [Either Error ValExpression] -> Either Error ValExpression
unsafeFunc ctx r vs = let fs = toFuncSignature r in
                        case partitionEithers vs of
                             ([], xs)   -> case lookupFunc fs ctx of
                                                Nothing -> error ("unsafeFunc: function can't be found in context - " ++ show fs)
                                                Just fd -> case view (body fd) of
                                                                Vconst x  -> unsafeConst x
                                                                _         -> case toMaybeValues xs of
                                                                                 Just _  -> let ps = paramDefs fd
                                                                                                in case addVars (toList ps) (fromFuncContext ctx) of
                                                                                                        Left e -> error ("unsafeFunc: can't make new context - " ++ show e)
                                                                                                        Right nctx -> subst nctx
                                                                                                                            (HashMap.fromList (zip (map (RefByName . name) (toList ps)) xs))
                                                                                                                            (body fd)
                                                                                 Nothing -> Right $ ValExpression (Vfunc r xs)
                             (es, _)    -> Left $ Error ("unsafeFunc Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))

