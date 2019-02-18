{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Function Context
-----------------------------------------------------------------------------
module TorXakis.FuncContext
( -- * Context
  -- ** Func Context class
  FuncContext (..)
  -- dependencies, yet part of interface
, module TorXakis.FuncSignatureContext
, FuncDef
)
where

import           TorXakis.FuncDef
import           TorXakis.FuncSignatureContext

-- | A FuncContext Context instance contains all definitions to work with 'TorXakis.FuncDef'.
class FuncSignatureContext a => FuncContext a where
    -- | lookup FuncDef
    lookupFunc :: FuncSignature -> a -> Maybe FuncDef
    -- | All FuncDef elements in the context
    elemsFunc :: a -> [FuncDef]
    -- | Add FuncDefs to func context.
    --   A func context is returned when the following constraints are satisfied:
    --
    --   * All func Signatures are distinct
    --
    --   * All references are known (sort, variables, func signatures)
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncs :: [FuncDef] -> a -> Either Error a
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
