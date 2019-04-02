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
module TorXakis.Subst
( -- * Substitution of variables
  subst
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
import           TorXakis.ValExpr.UnsafeFunc
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.Var

unsafeSubst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression
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
    unsafeSubstView (Vstrinre s r)            = unsafeStrInRe (unsafeSubstView (view s)) (unsafeSubstView (view r))
    unsafeSubstView (Vcstr a c l)             = unsafeCstr a c (map (unsafeSubstView . view) l)
    unsafeSubstView (Viscstr a c v)           = unsafeIsCstr a c (unsafeSubstView (view v))
    unsafeSubstView (Vaccess a c p v)         = unsafeAccess a c p (unsafeSubstView (view v))


-----------------------------------------------------------------------------
-- Substitute
-----------------------------------------------------------------------------
-- | find mismatches in sort in mapping
mismatchesSort :: VarContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> HashMap.Map (RefByName VarDef) ValExpression
mismatchesSort ctx = HashMap.filterWithKey mismatch
    where
        mismatch :: RefByName VarDef -> ValExpression -> Bool
        mismatch r e = case lookupVar (toName r) ctx of
                            Nothing -> error ("mismatchesSort - variable not defined in context - " ++ show r)
                            Just v  -> TorXakis.Var.sort v /= getSort ctx e

-- | Substitution: Substitute some variables by value expressions in a value expression.
-- The Either is needed since substitution can cause an invalid ValExpr.
-- For example, substitution of a variable by zero can cause a division by zero error
-- TODO: should we check the replacing val expressions? And the valExpression we get to work on?
-- TODO: should we support context shrinking, since by replacing the variable a by 10, the variable a might no longer be relevant in the context (and thus be removed)?
subst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression
subst ctx mp ve | HashMap.null mp               = Right ve
                | not (HashMap.null mismatches) = Left $ Error (T.pack ("Sort mismatches in map : " ++ show mismatches))
                | not (null undefinedVars)      = Left $ Error (T.pack ("Undefined variables in map : " ++ show undefinedVars))
                | otherwise                     = unsafeSubst ctx mp ve
  where
    mismatches :: HashMap.Map (RefByName VarDef) ValExpression
    mismatches = mismatchesSort ctx mp

    undefinedVars :: [RefByName VarDef]
    undefinedVars = filter (not . flip memberVar ctx . toName) (HashMap.keys mp)

-- | TODO: needed? Complete Substitution: Substitute all variables by value expressions in a value expression.
-- Since all variables are changed, one can change the kind of variables.
-- TODO: from one context to another
-- compSubst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression

-- | mkFuncOpt
-- Construct optimized function
-- This function should be preferred over mkFunc whenever a 'TorXakis.ValExprContext' is available
-- Note: since arguments of function can contain variables (e.g. the parameters of the calling function) a ValExprContext is needed (instead of a FuncContext)
mkFuncOpt :: ValExprContext c => c -> RefByFuncSignature -> [ValExpression] -> Either Error ValExpression
mkFuncOpt ctx r vs = case mkFunc ctx r vs of
                        Left e  -> Left e
                        Right v -> unsafeSubst ctx HashMap.empty v

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

