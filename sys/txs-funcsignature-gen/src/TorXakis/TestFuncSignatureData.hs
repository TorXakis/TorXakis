{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestFuncSignatureData
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- FuncSignature Data for Test: 
-- Additional data to ensure termination for QuickCheck
-----------------------------------------------------------------------------
module TorXakis.TestFuncSignatureData
(-- * Test FuncSignature Data
  TestFuncSignatureData
  -- dependencies, yet part of interface
, module TorXakis.TestSortData
)
where
import           TorXakis.TestSortData

-- | Test FuncSignature Data
type TestFuncSignatureData = TestSortData
