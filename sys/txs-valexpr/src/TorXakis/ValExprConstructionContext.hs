{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprConstructionContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Construction of ValExpressions.
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module TorXakis.ValExprConstructionContext
( -- * Context
  -- ** ValExpr Construction Context class
  ValExprConstructionContext
  -- dependencies, yet part of interface
, module TorXakis.FuncSignatureContext
, module TorXakis.VarContext
)
where
import           TorXakis.FuncSignatureContext
import           TorXakis.VarContext

-- | A ValExprConstructionContext Context instance contains all definitions to work with 'TorXakis.ValExpression'.
class (FuncSignatureContext c, VarContext c) => ValExprConstructionContext c