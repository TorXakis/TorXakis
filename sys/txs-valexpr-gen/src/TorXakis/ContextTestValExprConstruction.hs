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
            , tvecd :: TestValExprConstructionData
            }

-- | empty constructor
empty :: ContextTestValExprConstruction
empty = ContextTestValExprConstruction TorXakis.ContextValExprConstruction.empty TorXakis.TestValExprConstructionData.empty

-- | Constructor from ValExprConstructionContext
fromValExprConstructionContext :: ValExprConstructionContext b => b -> ContextTestValExprConstruction
fromValExprConstructionContext ctx = ContextTestValExprConstruction newContext newData
        where
            newContext :: ContextValExprConstruction
            newContext = case TorXakis.ValExprConstructionContext.addADTs (elemsADT ctx) ContextValExprConstruction.empty >>=
                              TorXakis.ValExprConstructionContext.addVars (elemsVar ctx) >>=
                              TorXakis.ValExprConstructionContext.addFuncSignatures (elemsFuncSignature ctx) of
                           Left e -> error ("Context should adhere to all constraint and thusmust be copyable, yet " ++ show e)
                           Right x -> x
            newData :: TestValExprConstructionData
            newData =   TorXakis.ValExprConstructionData.addFuncSignatures (elemsFuncSignature ctx) $
                        TorXakis.ValExprConstructionData.addVars (elemsVar ctx) $
                        TorXakis.ValExprConstructionData.addADTs (elemsADT ctx) TorXakis.ValExprConstructionData.empty


instance SortContext ContextTestValExprConstruction where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case addADTs as (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExprConstruction basis' (addADTs as (tvecd ctx))

instance TestSortContext ContextTestValExprConstruction where
    sortSize r = sortSize r . tvecd
    adtSize a = adtSize a . tvecd
    constructorSize a c = constructorSize a c . tvecd

instance VarContext ContextTestValExprConstruction where
    memberVar v = member v . basis

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

instance ValExprConstructionContext ContextTestValExprConstruction

instance TestValExprConstructionContext ContextTestValExprConstruction where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (_genMap ctx) s n of
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx


