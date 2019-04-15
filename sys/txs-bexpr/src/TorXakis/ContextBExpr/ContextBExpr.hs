{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for ValExpr: all defined sorts, variables, and functions
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExistentialQuantification #-}
module TorXakis.ContextBExpr.ContextBExpr
( -- * BExpr Context
  ContextBExpr (..)
, fromProcContext
)
where
import           TorXakis.BExprContext
import           TorXakis.NameMap

-- | An instance of 'TorXakis.BExprContext'.
data ContextBExpr = forall c . ProcContext c =>
                            ContextBExpr { _procContext :: c
                                         , varDefs :: NameMap VarDef
                                         , chanDefs :: NameMap ChanDef
                                         }

-- | Create ContextBExpr from ProcContext
fromProcContext :: ProcContext c => c -> ContextBExpr
fromProcContext pc = ContextBExpr pc TorXakis.NameMap.empty TorXakis.NameMap.empty

instance SortContext ContextBExpr where
    -- Can't use
    -- memberSort   = memberSort . procContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextBExpr ctx _ _) = memberSort r ctx

    memberADT r (ContextBExpr ctx _ _) = memberADT r ctx

    lookupADT r (ContextBExpr ctx _ _) = lookupADT r ctx

    elemsADT (ContextBExpr ctx _ _) = elemsADT ctx

    addADTs as (ContextBExpr ctx vs cs) = case addADTs as ctx of
                                                        Left e     -> Left e
                                                        Right nctx -> Right $ ContextBExpr nctx vs cs

instance FuncContext ContextBExpr where
    memberFunc f (ContextBExpr ctx _ _) = memberFunc f ctx

    lookupFunc f (ContextBExpr ctx _ _) = lookupFunc f ctx

    funcSignatures (ContextBExpr ctx _ _) = funcSignatures ctx

    elemsFunc (ContextBExpr ctx _ _) = elemsFunc ctx

    addFuncs fs (ContextBExpr ctx vs cs) = case addFuncs fs ctx of
                                            Left e     -> Left e
                                            Right nctx -> Right $ ContextBExpr nctx vs cs

instance ProcContext ContextBExpr where
    memberProc f (ContextBExpr ctx _ _) = memberProc f ctx

    lookupProc f (ContextBExpr ctx _ _) = lookupProc f ctx

    procSignatures (ContextBExpr ctx _ _) = procSignatures ctx

    elemsProc (ContextBExpr ctx _ _) = elemsProc ctx

    addProcs ps (ContextBExpr ctx vs cs) = case addProcs ps ctx of
                                            Left e     -> Left e
                                            Right nctx -> Right $ ContextBExpr nctx vs cs

instance VarContext ContextBExpr where
    memberVar v ctx = member v (varDefs ctx)

    lookupVar v ctx = TorXakis.NameMap.lookup v (varDefs ctx)

    elemsVar ctx    = elems (varDefs ctx)

    addVars vs ctx = case conceptualErrorAddVars vs ctx of
                                Just e  -> Left e
                                Nothing -> Right $ ctx {varDefs = toNameMap vs `union` varDefs ctx}


instance ValExprContext ContextBExpr

instance ChanContext ContextBExpr where
    memberChan v ctx = member v (chanDefs ctx)

    lookupChan v ctx = TorXakis.NameMap.lookup v (chanDefs ctx)

    elemsChan ctx    = elems (chanDefs ctx)

    addChans cs ctx = case conceptualErrorAddChans cs ctx of
                                Just e  -> Left e
                                Nothing -> Right $ ctx {chanDefs = toNameMap cs `union` chanDefs ctx}

instance BExprContext ContextBExpr
