{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestVarData
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Var Data for Test: 
-- Additional data to ensure termination for QuickCheck
-----------------------------------------------------------------------------
module TorXakis.TestVarData
(-- * Test Var Data
  TestVarData
, varSize
  -- dependencies, yet part of interface
, module TorXakis.TestSortData
)
where
import           TorXakis.Name
import           TorXakis.TestSortData
import           TorXakis.VarContext
import           TorXakis.Var

-- | Test Var Data
type TestVarData = TestSortData

-- | variable Size
--   The size of the provided var as specified by its name is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.Var' and any related 'TorXakis.Sort' references.
varSize :: VarContext c
        => RefByName VarDef 
        -> c
        -> TestVarData
        -> Int
varSize r ctx tvd = case lookupVar (toName r) ctx of 
                        Nothing -> error ("Reference " ++ show r ++ " does not refer to a variable in the provided context")
                        Just v  -> useSize (sort v)
        where
            useSize :: Sort -> Int
            useSize s = 1 + TorXakis.TestSortData.sortSize s tvd