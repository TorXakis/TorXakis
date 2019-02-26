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
  -- dependencies, yet part of interface
, module TorXakis.TestSortData
)
where
import           TorXakis.TestSortData

-- | Test Var Data
type TestVarData = TestSortData
