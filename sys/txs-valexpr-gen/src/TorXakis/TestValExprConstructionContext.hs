{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestValExprConstructionContext
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
module TorXakis.TestValExprConstructionContext
(-- * Test ValExpression Context
  TestValExprConstructionContext(..)
  -- dependencies, yet part of interface
, module TorXakis.ValExprConstructionContext
, module TorXakis.TestVarContext
, module TorXakis.TestFuncSignatureContext
)
where
import           Test.QuickCheck

import           TorXakis.SortGen
import           TorXakis.TestVarContext
import           TorXakis.TestFuncSignatureContext
import           TorXakis.ValExpr
import           TorXakis.ValExprConstructionContext


class (ValExprConstructionContext a, TestVarContext a, TestFuncSignatureContext a) => TestValExprConstructionContext a where
    arbitraryValExpr :: a -> Gen ValExpression
    arbitraryValExpr ctx = do
        s <- arbitrarySort ctx
        arbitraryValExprOfSort ctx s
    arbitraryValExprOfSort :: a -> Sort -> Gen ValExpression
