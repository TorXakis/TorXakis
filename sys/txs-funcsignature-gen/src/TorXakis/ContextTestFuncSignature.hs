{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTestFuncSignature
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- FuncSignature Context for Test: 
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextTestFuncSignature
(-- * Context Test FuncSignature 
  ContextTestFuncSignature
, TorXakis.ContextTestFuncSignature.empty
, TorXakis.ContextTestFuncSignature.fromSortContext
)
where
import           TorXakis.ContextFuncSignature
import           TorXakis.FuncSignatureContext
import           TorXakis.TestSortContext
import           TorXakis.TestFuncSignatureContext
import           TorXakis.TestFuncSignatureData

-- | An instance of 'TestFuncSignatureContext'.
data ContextTestFuncSignature = ContextTestFuncSignature { basis :: ContextFuncSignature
                                                         , tfd :: TestFuncSignatureData
                                                         }

-- | empty constructor
empty :: ContextTestFuncSignature
empty = ContextTestFuncSignature TorXakis.ContextFuncSignature.empty TorXakis.TestFuncSignatureData.empty

-- | Constructor from SortContext
fromSortContext :: SortContext b => b -> ContextTestFuncSignature
fromSortContext ctx = let nctx = TorXakis.ContextFuncSignature.fromSortContext ctx in
                        ContextTestFuncSignature nctx 
                                                (TorXakis.TestFuncSignatureData.afterAddADTs nctx (elemsADT ctx) TorXakis.TestFuncSignatureData.empty)

instance SortContext ContextTestFuncSignature where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case TorXakis.FuncSignatureContext.addADTs as (basis ctx) of
                          Left e        -> Left e
                          Right basis'  -> Right $ ContextTestFuncSignature basis' (TorXakis.TestFuncSignatureData.afterAddADTs basis' as (tfd ctx))

instance TestSortContext ContextTestFuncSignature where
    sortSize s = TorXakis.TestFuncSignatureData.sortSize s . tfd

    adtSize r = TorXakis.TestFuncSignatureData.adtSize r . tfd

    constructorSize r c = TorXakis.TestFuncSignatureData.constructorSize r c . tfd

instance FuncSignatureContext ContextTestFuncSignature where
    memberFunc f = memberFunc f . basis

    funcSignatures = funcSignatures . basis

instance FuncSignatureModifyContext ContextTestFuncSignature ContextTestFuncSignature where
    addFuncSignatures fs ctx = case addFuncSignatures fs (basis ctx) of
                                    Left e       -> Left e
                                    Right basis' -> Right $ ctx {basis = basis'}

instance TestFuncSignatureContext ContextTestFuncSignature