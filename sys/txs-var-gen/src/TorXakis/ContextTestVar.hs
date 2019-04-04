{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTestVar
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Var Context for Test: 
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
module TorXakis.ContextTestVar
(-- * Context Test Var
  ContextTestVar
, TorXakis.ContextTestVar.empty
, TorXakis.ContextTestVar.fromSortContext
, arbitraryContextTestVar
)
where
import           Test.QuickCheck

import           TorXakis.ContextVar
import           TorXakis.VarContext
import           TorXakis.SortGen
import           TorXakis.TestVarContext
import           TorXakis.TestVarData
import           TorXakis.Var
import           TorXakis.VarsDeclGen

-- | An instance of 'TestVarContext'.
data ContextTestVar = ContextTestVar { basis :: ContextVar
                                     , tvd :: TestVarData
                                     }

-- | empty constructor
empty :: ContextTestVar
empty = ContextTestVar TorXakis.ContextVar.empty TorXakis.TestVarData.empty

-- | Constructor from SortContext
fromSortContext :: SortContext b => b -> ContextTestVar
fromSortContext ctx = let nctx = TorXakis.ContextVar.fromSortContext ctx in
                        ContextTestVar nctx (TorXakis.TestVarData.afterAddADTs nctx (elemsADT ctx) TorXakis.TestVarData.empty)

instance SortContext ContextTestVar where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case TorXakis.VarContext.addADTs as (basis ctx) of
                          Left e        -> Left e
                          Right basis'  -> Right $ ContextTestVar basis' (TorXakis.TestVarData.afterAddADTs basis' as (tvd ctx))

instance TestSortContext ContextTestVar where
    sortSize s = TorXakis.TestVarData.sortSize s . tvd

    adtSize r = TorXakis.TestVarData.adtSize r . tvd

    constructorSize r c = TorXakis.TestVarData.constructorSize r c . tvd

instance VarContext ContextTestVar where
    memberVar v = memberVar v . basis

    lookupVar v = lookupVar v . basis

    elemsVar  = elemsVar . basis

    addVars vs ctx = case addVars vs (basis ctx) of
                          Left e     -> Left e
                          Right basis' -> Right $ ctx {basis = basis'}

instance TestVarContext ContextTestVar where
    varSize r ctx = TorXakis.TestVarData.varSize r (basis ctx) (tvd ctx)
    
-- | generate an arbitrary Test Var Context
arbitraryContextTestVar :: Gen ContextTestVar
arbitraryContextTestVar =
    let ctx1 = TorXakis.ContextTestVar.empty in do
        incr <- arbitraryADTDefs ctx1
        case addADTs incr ctx1 of
            Left e    -> error ("arbitraryContextTestVar: Invalid generator - addADTs " ++ show e)
            Right ctx2 -> do
                            vs <- arbitraryVarsDecl ctx2
                            case addVars (toList vs) ctx2 of
                                Left e     -> error ("arbitraryContextTestVar: Invalid generator - addVars " ++ show e)
                                Right ctx3 -> return ctx3
