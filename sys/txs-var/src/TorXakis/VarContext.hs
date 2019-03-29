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
, conceptualErrorAddVars
  -- dependencies, yet part of interface
, module TorXakis.SortContext
, VarDef
)
where
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.SortContext
import           TorXakis.Var

-- | A Variable Context instance contains all definitions to work with sorts and variables
    -- TODO? addVarsDecl function -> don't need to check uniqueness of added variables (still need to check for undefined sorts).
class SortContext c => VarContext c where
    -- | Refers the provided VarDef name to a VarDef in the context?
    memberVar :: Name -> c -> Bool
    -- | lookup VarDef
    lookupVar :: Name -> c -> Maybe VarDef
    -- | All VarDef elements in the context
    elemsVar :: c -> [VarDef]
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
    addVars :: [VarDef] -> c -> Either Error c

-- | Validation function that reports whether a conceptual error will occur
--   when the list of 'VarDef's would be added to the given context.
--   The conceptual error reflects the violations of any of the following constraints:
--
--   * The 'Name's of the added VarDef are unique
--
--   * All references are known
conceptualErrorAddVars :: VarContext c => [VarDef] -> c -> Maybe Error
conceptualErrorAddVars vs ctx | not $ null nuVarDefs        = Just $ Error ("Non unique variable definitions: " ++ show nuVarDefs)
                              | not $ null undefinedSorts   = Just $ Error ("List of variable definitions with undefined sorts: " ++ show undefinedSorts)
                              | otherwise                   = Nothing
    where
            -- | non unique Variable Definitions (i.e. duplicate names)
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        -- | undefined Sorts of Variable Definitions.
        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . flip memberSort ctx . sort) vs