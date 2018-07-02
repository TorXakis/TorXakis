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
module ValExpr
( ValExpr
, view
, eval
, ValExprView(..)
, PredefKind(..)
, Resettable
, module ValExprImpls
, module ValExprImplsExtension
)
where

import           Id
import           ValExprDefs
import           ValExprImpls
import           ValExprImplsExtension
