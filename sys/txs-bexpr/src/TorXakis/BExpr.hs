{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface file for Behavioural Expressions.
-----------------------------------------------------------------------------
module TorXakis.BExpr
( -- * Behaviour Expression type and view
  BExprView
, BExpressionView (..)
, BExpr
, view
  -- Action Offer
, ActOffer(..)
  -- Channel Offer
, ChanOffer(..)
, module TorXakis.BExpr.BExprContext
)
where

import TorXakis.BExpr.BExpr
import TorXakis.BExpr.BExprContext
