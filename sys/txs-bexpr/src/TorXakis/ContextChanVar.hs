{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextChanVar
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Chan and Var: all defined sorts, variables, and channels
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExistentialQuantification #-}
module TorXakis.ContextChanVar
( -- * ChanVar Context
  ContextChanVar (..)
, empty
, fromSortContext
, fromVarContext
  -- dependencies, yet part of interface
, module TorXakis.ContextVar
, module TorXakis.ChanContext
)
where
import           TorXakis.ContextVar   hiding (empty, fromSortContext)
import qualified TorXakis.ContextVar
import           TorXakis.Chan
import           TorXakis.ChanContext
import           TorXakis.NameMap      hiding (empty)
import qualified TorXakis.NameMap


-- | An instance of 'TorXakis.ChanContext' and 'TorXakis.VarContext'.
data ContextChanVar = forall c . VarContext c =>
                            ContextChanVar { _varContext :: c
                                           , chanDefs :: NameMap ChanDef
                                           }

-- | empty
empty :: ContextChanVar
empty = fromVarContext TorXakis.ContextVar.empty

-- | Constructor from SortContext
fromSortContext :: SortContext c => c -> ContextChanVar
fromSortContext ctx = fromVarContext (TorXakis.ContextVar.fromSortContext ctx)

-- | Create ContextChanVar from VarContext
fromVarContext :: VarContext c => c -> ContextChanVar
fromVarContext vc = ContextChanVar vc TorXakis.NameMap.empty

instance SortContext ContextChanVar where
    -- Can't use
    -- memberSort   = memberSort . procContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextChanVar ctx _) = memberSort r ctx

    memberADT r (ContextChanVar ctx _) = memberADT r ctx

    lookupADT r (ContextChanVar ctx _) = lookupADT r ctx

    elemsADT (ContextChanVar ctx _) = elemsADT ctx

    addADTs as (ContextChanVar ctx cs) = case addADTs as ctx of
                                              Left e     -> Left e
                                              Right nctx -> Right $ ContextChanVar nctx cs

instance VarContext ContextChanVar where
    memberVar r (ContextChanVar ctx _) = memberVar r ctx
    
    lookupVar r (ContextChanVar ctx _) = lookupVar r ctx
    
    elemsVar (ContextChanVar ctx _) = elemsVar ctx
    
    addVars vs (ContextChanVar ctx cs) = case addVars vs ctx of
                                              Left e     -> Left e
                                              Right nctx -> Right $ ContextChanVar nctx cs

instance ChanContext ContextChanVar where
    memberChan v ctx = member v (chanDefs ctx)

    lookupChan v ctx = TorXakis.NameMap.lookup v (chanDefs ctx)

    elemsChan ctx    = elems (chanDefs ctx)

    addChans cs ctx = case conceptualErrorAddChans cs ctx of
                                Just e  -> Left e
                                Nothing -> Right $ ctx {chanDefs = toNameMap cs `union` chanDefs ctx}

