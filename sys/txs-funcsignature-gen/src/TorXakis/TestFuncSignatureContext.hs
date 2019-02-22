{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestFuncSignatureContext
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
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.TestFuncSignatureContext
(-- * Test FuncSignature Context
  TestFuncSignatureContext (..)
, ContextTestFuncSignature
, fromTestSortContext
  -- dependencies, yet part of interface
, module TorXakis.TestSortContext
, module TorXakis.FuncSignatureContext
)
where
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.ContextFuncSignatureExposed
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext
import           TorXakis.TestSortContext


-- | A TestFuncSignatureContext instance contains all definitions to work with funcSignatures and reference thereof for test purposes
class (TestSortContext a, FuncSignatureContext a) => TestFuncSignatureContext a where
    -- | FuncSignature Size
    --   The size of the provided funcSignature as specified by the references to 'TorXakis.FuncSignature' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.FuncSignature' and any related 'TorXakis.Sort' references.
    funcSize :: a -> FuncSignature -> Int
    funcSize ctx f = sum (useSize (returnSort f): map useSize (args f))
        where
            useSize :: Sort -> Int
            useSize s = 1 + sortSize s ctx

-- | An instance of 'TestFuncSignatureContext'.
newtype ContextTestFuncSignature = ContextTestFuncSignature { basis :: ContextFuncSignatureExposed ContextTestSort }
                                                            deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor
fromTestSortContext :: ContextTestSort -> ContextTestFuncSignature
fromTestSortContext = ContextTestFuncSignature . fromSortContext

instance SortContext ContextTestFuncSignature where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case addADTs as (basis ctx) of
                          Left e     -> Left e
                          Right ctx' -> Right $ ctx {basis = ctx'}

instance TestSortContext ContextTestFuncSignature where
    sortSize r = sortSize r . toSortContext . basis
    
    adtSize a = adtSize a . toSortContext . basis
    
    constructorSize a c = constructorSize a c . toSortContext . basis

instance FuncSignatureContext ContextTestFuncSignature where
    memberFunc f = memberFunc f . basis

    funcSignatures = funcSignatures . basis

instance FuncSignatureModifyContext ContextTestFuncSignature ContextTestFuncSignature where
    addFuncSignatures fs ctx = case addFuncSignatures fs (basis ctx) of
                                    Left e     -> Left e
                                    Right ctx' -> Right $ ctx {basis = ctx'}


instance TestFuncSignatureContext ContextTestFuncSignature