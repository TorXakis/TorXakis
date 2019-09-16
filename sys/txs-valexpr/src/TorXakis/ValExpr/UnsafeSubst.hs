{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  UnsafeSubst
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
module TorXakis.ValExpr.UnsafeSubst
( unsafeSubst
)
where
import           Control.Arrow          (first)
import           Data.Either
import qualified Data.HashMap           as HashMap
import           Data.List
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import           TorXakis.ContextValExpr.ContextValExpr
import           TorXakis.Error
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.ValExpr.Unsafe
import           TorXakis.ValExpr.ValExpr
import           TorXakis.Var

-- | unsafe Substitution
-- only for usage within the ValExpr package.
-- This function should not be exported.
unsafeSubst :: FuncContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression
unsafeSubst ctx  mp = unsafeSubstView . view
  where
    unsafeSubstView :: ValExpressionView -> Either Error ValExpression
    unsafeSubstView (Vconst c)                = unsafeConst c
    unsafeSubstView (Vvar r)                  = case HashMap.lookup r mp of
                                                    Nothing -> unsafeVar r
                                                    Just x  -> Right x
    unsafeSubstView (Vequal ve1 ve2)          = unsafeEqual (unsafeSubstView (view ve1)) (unsafeSubstView (view ve2))
    unsafeSubstView (Vite c tb fb)            = unsafeITE (unsafeSubstView (view c)) (unsafeSubstView (view tb)) (unsafeSubstView (view fb))
    unsafeSubstView (Vfunc r vs)              = unsafeFunc ctx r (map (unsafeSubstView . view) vs)
    unsafeSubstView (Vpredef r vs)            = unsafePredefNonSolvable ctx r (map (unsafeSubstView . view) vs)
    unsafeSubstView (Vnot v)                  = unsafeNot (unsafeSubstView (view v))
    unsafeSubstView (Vand s)                  = unsafeAnd (Set.map (unsafeSubstView . view) s)
    unsafeSubstView (Vdivide t n)             = unsafeDivide (unsafeSubstView (view t)) (unsafeSubstView (view n))
    unsafeSubstView (Vmodulo t n)             = unsafeModulo (unsafeSubstView (view t)) (unsafeSubstView (view n))
    unsafeSubstView (Vsum m)                  = unsafeSumFactor (map (first (unsafeSubstView . view)) (Map.toList m))
    unsafeSubstView (Vproduct m)              = unsafeProductFactor (map (first (unsafeSubstView . view)) (Map.toList m))
    unsafeSubstView (Vgez v)                  = unsafeGEZ (unsafeSubstView (view v))
    unsafeSubstView (Vlength s)               = unsafeLength (unsafeSubstView (view s))
    unsafeSubstView (Vat s i)                 = unsafeAt (unsafeSubstView (view s)) (unsafeSubstView (view i))
    unsafeSubstView (Vconcat s)               = unsafeConcat (map (unsafeSubstView . view) s)
    unsafeSubstView (Vstrinre s r)            = unsafeStrInRe (unsafeSubstView (view s)) r
    unsafeSubstView (Vcstr a c l)             = unsafeCstr a c (map (unsafeSubstView . view) l)
    unsafeSubstView (Viscstr a c v)           = unsafeIsCstr a c (unsafeSubstView (view v))
    unsafeSubstView (Vaccess a c p v)         = unsafeAccess a c p (unsafeSubstView (view v))
    unsafeSubstView (Vforall _ _)            = undefined -- TODO

-- TODO? More laziness?
-- e.g. depending on some parameter value, some other parameter values might be irrelevant
-- e.g. ANY/Error for not initialized variables of State Automaton translated to a ProcDef
unsafeFunc :: FuncContext c => c -> RefByFuncSignature -> [Either Error ValExpression] -> Either Error ValExpression
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
                                                                                                        Right nctx -> unsafeSubst nctx
                                                                                                                                  (HashMap.fromList (zip (map (RefByName . name) (toList ps)) xs))
                                                                                                                                  (body fd)
                                                                                 Nothing -> Right $ ValExpression (Vfunc r xs)
                             (es, _)    -> Left $ Error ("unsafeFunc Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))

