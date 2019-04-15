{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextBExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface file for a Context for Behaviour Expressions.
-----------------------------------------------------------------------------
module TorXakis.ContextBExpr
(  -- * Context
  -- ** instance of ValExpr Context
  ContextBExpr
, fromProcContext
, module TorXakis.ContextBExpr.ContextBExprBasis
  -- dependencies, yet part of interface
, module TorXakis.BExprContext
)
where
import TorXakis.ContextBExpr.ContextBExpr
import TorXakis.ContextBExpr.ContextBExprBasis
import TorXakis.BExprContext