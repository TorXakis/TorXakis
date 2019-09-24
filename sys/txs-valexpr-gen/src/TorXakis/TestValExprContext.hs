{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestValExprContext
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
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.TestValExprContext
(-- * Test ValExpression Context
  TestValExprContext(..)
  -- dependencies, yet part of interface
, module TorXakis.ValExprContext
, module TorXakis.TestVarContext
, RefByFuncSignature
)
where
import           Test.QuickCheck

import           TorXakis.FuncSignature
import           TorXakis.SortGen
import           TorXakis.TestVarContext
import           TorXakis.ValExpr
import           TorXakis.ValExprContext

-- | Class for TestValExprConstructionContext
class (ValExprContext c, TestVarContext c) => TestValExprContext c where
    arbitraryValExpr :: c -> Gen ValExpression
    arbitraryValExpr ctx = do
        s <- arbitrarySort ctx
        arbitraryValExprOfSort ctx s
    arbitraryValExprOfSort :: c -> Sort -> Gen ValExpression

    -- | FuncSignature Size
    --   The size of the provided funcSignature as specified by the references to 'TorXakis.FuncSignature' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.FuncSignature' and any related 'TorXakis.Sort' references.
    funcSize :: RefByFuncSignature -> c -> Int