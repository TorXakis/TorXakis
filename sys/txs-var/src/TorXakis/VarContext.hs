{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Variables.
-----------------------------------------------------------------------------
module TorXakis.VarContext
( -- * Context
  -- ** Variable Context class
  VarContext (..)
  -- dependencies, yet part of interface
, module TorXakis.SortContext
, VarDef
)
where
import           TorXakis.SortContext
import           TorXakis.Var

-- | A Variable Context instance contains all definitions to work with sorts and variables
    -- TODO? addVarsDecl function -> don't need to check uniqueness of added variables (still need to check for undefined sorts).
class SortContext a => VarContext a where
    -- | Refers the provided VarDef name to a VarDef in the context?
    memberVar :: Name -> a -> Bool
    -- | lookup VarDef
    lookupVar :: Name -> a -> Maybe VarDef
    -- | All VarDef elements in the context
    elemsVar :: a -> [VarDef]
    -- | Add variables to variable context.
    --   A variable context is returned when the following constraints are satisfied:
    --
    --   * The references of the added variables are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    -- Note: added variables might hide previously defined variables.
    -- Note: the order of the variables is not relevant.
    addVars :: [VarDef] -> a -> Either Error a
    -- | Replace variables of variable context.
    --   A variable context is returned when the following constraints are satisfied:
    --
    --   * The references of the variables are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    replaceVars :: [VarDef] -> a -> Either Error a
