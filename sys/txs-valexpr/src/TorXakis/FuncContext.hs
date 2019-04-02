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
, module TorXakis.SortContext
, FuncDef
, FuncSignature
)
where

import           TorXakis.FuncSignature
import           TorXakis.FuncDef
import           TorXakis.SortContext

-- | A FuncContext Context instance contains all definitions to work with 'TorXakis.FuncDef'.
class SortContext c => FuncContext c where
    -- | Points the provided FuncSignature to a FunctionDefinition in the context?
    memberFunc :: FuncSignature -> c -> Bool
    -- | lookup FuncDef using the provided function signature.
    lookupFunc :: FuncSignature -> c -> Maybe FuncDef
    -- | All funcSignatures in the context.
    -- 
    -- Since all funcSignatures are distinct the following properties hold:
    --
    -- prop> List.nub (funcSignatures x) == funcSignatures x
    -- 
    -- prop> Set.toList (Set.fromList (funcSignatures x)) == funcSignatures x
    funcSignatures :: c -> [FuncSignature]
    -- | All FuncDef elements in the context
    elemsFunc :: c -> [FuncDef]
    -- | Add FuncDefs to func context.
    --   A func context is returned when the following constraints are satisfied:
    --
    --   * All func Signatures are distinct
    --
    --   * All references are known (sort, variables, func signatures)
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncs :: [FuncDef] -> c -> Either Error c

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
