{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface file for a Context for Value Expressions.
-----------------------------------------------------------------------------
module TorXakis.ContextValExpr
(  -- * Context
  -- ** instance of ValExpr Context
  ContextValExpr
, fromFuncContext
, module TorXakis.ContextValExpr.ContextValExprBasis
  -- dependencies, yet part of interface
, module TorXakis.ValExprContext
)
where
import TorXakis.ContextValExpr.ContextValExpr
import TorXakis.ContextValExpr.ContextValExprBasis
import TorXakis.ValExprContext