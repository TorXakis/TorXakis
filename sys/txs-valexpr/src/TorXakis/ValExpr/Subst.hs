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
module TorXakis.ValExpr.Subst
( -- * Substitution of variables
  subst
    -- * Constructor for optimize ValExpression given a FuncContext
, mkFuncOpt

)
where
import qualified Data.HashMap           as HashMap
import qualified Data.Text              as T

import           TorXakis.ContextValExpr
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.ValExpr.UnsafeSubst
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.Var

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
