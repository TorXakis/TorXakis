{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface file for Value Expressions.
-----------------------------------------------------------------------------
module TorXakis.ValExpr
( -- * Value Expression
  ValExprView (..)
, ValExpr
, view
  -- * Evaluate
, eval
, PredefKind(..)
, module TorXakis.ValExpr.ValExprConstructor
, module TorXakis.ValExpr.ValExprExtension
, module TorXakis.ValExpr.ValExprContext
)
where

import TorXakis.ValExpr.ValExpr
import TorXakis.ValExpr.ValExprContext
import TorXakis.ValExpr.ValExprConstructor
import TorXakis.ValExpr.ValExprExtension
