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
)
where
--import           Debug.Trace

import qualified Data.List           as List
import qualified Data.Text           as T
import           Test.QuickCheck

import           TorXakis.ContextValExprConstruction
import           TorXakis.Error
import           TorXakis.FuncContext
import qualified TorXakis.GenCollection
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.Sort
import           TorXakis.SortGen
import           TorXakis.TestValExprConstructionContext
import           TorXakis.TestValExprConstructionData
import           TorXakis.ValExpr
import           TorXakis.ValExprConstructionContext
import           TorXakis.Value
import           TorXakis.ValueGen
import           TorXakis.VarContext
import           TorXakis.Var

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
            newData =   TorXakis.TestValExprConstructionData.afterAddFuncSignatures (funcSignatures ctx) $
                        TorXakis.TestValExprConstructionData.afterAddVars (elemsVar ctx) $
                        TorXakis.TestValExprConstructionData.afterAddADTs (elemsADT ctx) TorXakis.TestValExprConstructionData.empty


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

    lookupVar v = TorXakis.NameMap.lookup v basis

    elemsVar = elemsVar . basis

    addVars vs ctx = case addVars vs (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExprConstruction basis' (addVars vs (tvecd ctx))

instance TestVarContext ContextTestValExprConstruction

instance FuncSignatureContext ContextTestValExprConstruction where

instance TestFuncSignatureContext ContextTestValExprConstruction

instance FuncContext ContextTestValExprConstruction where

instance ValExprConstructionContext ContextTestValExprConstruction

instance TestValExprConstructionContext ContextTestValExprConstruction where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (genMap (tvecd ctx)) s n of                 -- TODO: hide map like sortSize?
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx

