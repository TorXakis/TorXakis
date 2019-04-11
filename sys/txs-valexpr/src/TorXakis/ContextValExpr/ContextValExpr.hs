{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExpr
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
module TorXakis.ContextValExpr.ContextValExpr
( -- * Context
  -- ** instance of ValExpr Context
  ContextValExpr (..)
, fromFuncContext
  -- dependencies, yet part of interface
, module TorXakis.ValExprContext
)
where
import           TorXakis.NameMap
import           TorXakis.ValExprContext


-- | An instance of 'TorXakis.ValExprConstructionContext'.
data ContextValExpr = forall c . FuncContext c =>
                            ContextValExpr { _funcContext :: c
                                           , varDefs :: NameMap VarDef
                                           }

-- | Create ContextValExpr from FuncContext
fromFuncContext :: FuncContext c => c -> ContextValExpr
fromFuncContext fc = ContextValExpr fc TorXakis.NameMap.empty

instance SortContext ContextValExpr where
    -- Can't use
    -- memberSort   = memberSort . funcContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextValExpr ctx _) = memberSort r ctx

    memberADT r (ContextValExpr ctx _) = memberADT r ctx

    lookupADT r (ContextValExpr ctx _) = lookupADT r ctx

    elemsADT (ContextValExpr ctx _) = elemsADT ctx

    addADTs as (ContextValExpr ctx vs) = case addADTs as ctx of
                                                        Left e     -> Left e
                                                        Right sctx -> Right $ ContextValExpr sctx vs

instance VarContext ContextValExpr where
    memberVar v ctx = member v (varDefs ctx)

    lookupVar v ctx = TorXakis.NameMap.lookup v (varDefs ctx)

    elemsVar ctx    = elems (varDefs ctx)

    addVars vs ctx = case conceptualErrorAddVars vs ctx of
                                Just e  -> Left e
                                Nothing -> Right $ ctx {varDefs = toNameMap vs `union` varDefs ctx}

instance FuncContext ContextValExpr where
    memberFunc f (ContextValExpr ctx _) = memberFunc f ctx

    lookupFunc f (ContextValExpr ctx _) = lookupFunc f ctx

    funcSignatures (ContextValExpr ctx _) = funcSignatures ctx

    elemsFunc (ContextValExpr ctx _) = elemsFunc ctx

    addFuncs vs (ContextValExpr ctx fs) = case addFuncs vs ctx of
                                            Left e     -> Left e
                                            Right sctx -> Right $ ContextValExpr sctx fs

instance ValExprContext ContextValExpr
