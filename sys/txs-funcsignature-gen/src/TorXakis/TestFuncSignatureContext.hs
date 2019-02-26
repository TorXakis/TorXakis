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
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.TestFuncSignatureContext
(-- * Test FuncSignature Context
  TestFuncSignatureContext (..)
  -- dependencies, yet part of interface
, module TorXakis.TestSortContext
, module TorXakis.FuncSignatureContext
)
where
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext
import           TorXakis.TestSortContext

-- | A TestFuncSignatureContext instance contains all definitions to work with funcSignatures and reference thereof for test purposes
class (TestSortContext a, FuncSignatureContext a) => TestFuncSignatureContext a where
    -- | FuncSignature Size
    --   The size of the provided funcSignature as specified by the references to 'TorXakis.FuncSignature' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.FuncSignature' and any related 'TorXakis.Sort' references.
    funcSize :: a -> RefByFuncSignature -> Int
    funcSize ctx r = let f = toFuncSignature r in
                        sum (useSize (returnSort f): map useSize (args f))
        where
            useSize :: Sort -> Int
            useSize s = 1 + TorXakis.TestSortContext.sortSize s ctx
