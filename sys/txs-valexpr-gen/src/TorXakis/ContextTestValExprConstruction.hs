{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTestValExprConstruction
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- ValExpression Context for Test: 
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ContextTestValExprConstruction
(-- * Context Test ValExpression
  ContextTestValExprConstruction(..)
, TorXakis.ContextTestValExprConstruction.empty
, fromValExprConstructionContext
)
where
--import           Debug.Trace

import           Test.QuickCheck

import           TorXakis.ContextValExprConstruction
import           TorXakis.FuncContext
import qualified TorXakis.GenCollection
import           TorXakis.SortGen
import           TorXakis.TestValExprConstructionContext
import           TorXakis.TestValExprConstructionData

-------------------------------------------------------------------------------------------------------------
-- Test Func Context
-------------------------------------------------------------------------------------------------------------

data ContextTestValExprConstruction = 
        ContextTestValExprConstruction 
            { basis :: ContextValExprConstruction
            , tvecd :: TestValExprConstructionData ContextTestValExprConstruction
            }

-- | empty constructor
empty :: ContextTestValExprConstruction
empty = let ctx = TorXakis.ContextValExprConstruction.empty in
            ContextTestValExprConstruction ctx (TorXakis.TestValExprConstructionData.empty ctx)

-- | Constructor from ValExprConstructionContext
fromValExprConstructionContext :: ValExprConstructionContext b => b -> ContextTestValExprConstruction
fromValExprConstructionContext ctx = ContextTestValExprConstruction newContext newData
        where
            newContext :: ContextValExprConstruction
            newContext = case TorXakis.TestValExprConstructionContext.addADTs (elemsADT ctx) TorXakis.ContextValExprConstruction.empty >>=
                              TorXakis.TestValExprConstructionContext.addVars (elemsVar ctx) >>=
                              TorXakis.TestValExprConstructionContext.addFuncSignatures (funcSignatures ctx) of
                           Left e -> error ("Context should adhere to all constraint and thusmust be copyable, yet " ++ show e)
                           Right x -> x
            newData :: TestValExprConstructionData ContextTestValExprConstruction
            newData =   TorXakis.TestValExprConstructionData.afterAddFuncSignatures ctx (funcSignatures ctx) $
                        TorXakis.TestValExprConstructionData.afterAddVars ctx (elemsVar ctx) $
                        TorXakis.TestValExprConstructionData.afterAddADTs ctx (elemsADT ctx) (TorXakis.TestValExprConstructionData.empty ctx)


instance SortContext ContextTestValExprConstruction where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case addADTs as (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExprConstruction basis' (TorXakis.TestValExprConstructionData.afterAddADTs basis' as (tvecd ctx))

instance TestSortContext ContextTestValExprConstruction where
    sortSize r = TorXakis.TestValExprConstructionData.sortSize r . tvecd
    adtSize a = TorXakis.TestValExprConstructionData.adtSize a . tvecd
    constructorSize a c = TorXakis.TestValExprConstructionData.constructorSize a c . tvecd

instance VarContext ContextTestValExprConstruction where
    memberVar v = memberVar v . basis

    lookupVar v = lookupVar v . basis

    elemsVar = elemsVar . basis

    addVars vs ctx = case addVars vs (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExprConstruction basis' (TorXakis.TestValExprConstructionData.afterAddVars basis' vs (tvecd ctx))

instance TestVarContext ContextTestValExprConstruction where
    varSize r ctx = TorXakis.TestValExprConstructionData.varSize r (basis ctx) (tvecd ctx)

instance FuncSignatureContext ContextTestValExprConstruction where
    memberFunc r = memberFunc r . basis

    funcSignatures = funcSignatures . basis

instance TestFuncSignatureContext ContextTestValExprConstruction where
    funcSize r ctx = TorXakis.TestValExprConstructionData.funcSize r (basis ctx) (tvecd ctx)

instance ValExprConstructionContext ContextTestValExprConstruction

instance TestValExprConstructionContext ContextTestValExprConstruction where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (genMap (tvecd ctx)) s n of                 -- TODO: hide map like sortSize?
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx

