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
module TorXakis.TestVarContext
(-- * Test Var Context
  TestVarContext (..)
  -- dependencies, yet part of interface
, module TorXakis.TestSortContext
, module TorXakis.VarContext
)
where
import           TorXakis.Name
import           TorXakis.Var
import           TorXakis.VarContext
import           TorXakis.TestSortContext

-- | A TestVarContext instance contains all definitions to work with vars and reference thereof for test purposes
class (TestSortContext c, VarContext c) => TestVarContext c where
    -- | Var Size
    --   The size of the provided var as specified by its name is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.Var' and any related 'TorXakis.Sort' references.
    varSize :: RefByName VarDef -> c -> Int
