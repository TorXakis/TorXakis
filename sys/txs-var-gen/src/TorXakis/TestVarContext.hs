{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestVarContext
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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.TestVarContext
(-- * Test Var Context
  TestVarContext (..)
, ContextTestVar
, fromTestSortContext
  -- dependencies, yet part of interface
, module TorXakis.TestSortContext
, module TorXakis.VarContext
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.ContextVarExposed
import           TorXakis.Var
import           TorXakis.VarContext
import           TorXakis.TestSortContext


-- | A TestVarContext instance contains all definitions to work with vars and reference thereof for test purposes
class (TestSortContext a, VarContext a) => TestVarContext a where
    -- | Var Size
    --   The size of the provided var as specified by the references to 'TorXakis.Var' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.Var' and any related 'TorXakis.Sort' references.
    varSize :: a -> RefByName VarDef -> Int
    varSize ctx r = case lookupVar ctx r of 
                        Nothing -> error ("Reference " ++ show r ++ " does not refer to a variable in the provided context")
                        Just v  -> useSize (sort v)
        where
            useSize :: Sort -> Int
            useSize s = 1 + sortSize ctx s

-- | An instance of 'TestVarContext'.
newtype ContextTestVar = ContextTestVar { basis :: ContextVarExposed ContextTestSort }
                                        deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Constructor
fromTestSortContext :: ContextTestSort -> ContextTestVar
fromTestSortContext = ContextTestVar . fromSortContext

instance SortContext ContextTestVar where
    memberSort   = memberSort . basis

    memberADT = memberADT . basis

    lookupADT = lookupADT . basis

    elemsADT  = elemsADT . basis

    addADTs ctx as = case addADTs (basis ctx) as of
                          Left e     -> Left e
                          Right ctx' -> Right $ ctx {basis = ctx'}

instance TestSortContext ContextTestVar where
    sortSize = sortSize . toSortContext . basis

    adtSize = adtSize . toSortContext . basis

    constructorSize = constructorSize . toSortContext . basis

instance VarContext ContextTestVar where
    memberVar = memberVar . basis

    lookupVar = lookupVar . basis

    elemsVar  = elemsVar . basis

    addVars ctx as = case addVars (basis ctx) as of
                          Left e     -> Left e
                          Right ctx' -> Right $ ctx {basis = ctx'}

    replaceVars ctx as = case replaceVars (basis ctx) as of
                          Left e     -> Left e
                          Right ctx' -> Right $ ctx {basis = ctx'}

instance TestVarContext ContextTestVar