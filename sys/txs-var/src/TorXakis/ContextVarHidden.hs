{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextVarHidden
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Variables.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module TorXakis.ContextVarHidden
( -- * Context Variable instance
  ContextVarHidden
, fromSortContext
)
where
import qualified Data.HashMap    as HashMap

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.SortContext
import           TorXakis.VarContext
import           TorXakis.VarDef

-- | An instance of 'TorXakis.VarContext'.
data ContextVarHidden = forall a . SortContext a => 
                            ContextVarHidden { _sortContext :: a -- use _ to prevent warning "Defined but not used: `sortContext'"
                                               -- variable definitions
                                             , varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                             }
-- | Constructor from SortContext
fromSortContext :: SortContext b => b -> ContextVarHidden
fromSortContext ctx = ContextVarHidden ctx HashMap.empty

instance SortContext ContextVarHidden where
    -- Can't use
    -- memberSort   = memberSort . sortContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort (ContextVarHidden ctx _) = memberSort ctx

    memberADT (ContextVarHidden ctx _) = memberADT ctx

    lookupADT (ContextVarHidden ctx _) = lookupADT ctx

    elemsADT (ContextVarHidden ctx _) = elemsADT ctx

    addADTs (ContextVarHidden ctx vs) as = case addADTs ctx as of
                                                Left e     -> Left e
                                                Right sctx -> Right $ ContextVarHidden sctx vs

-- | non unique Variable Definitions (i.e. duplicate names)
nuVarDefs :: [VarDef] -> [VarDef]
nuVarDefs = repeatedByName

-- | undefined Sorts of Variable Definitions.
undefinedSorts :: SortContext a => a -> [VarDef] -> [VarDef]
undefinedSorts ctx = filter (not . memberSort ctx . sort)

instance VarContext ContextVarHidden where
    memberVar ctx v = HashMap.member v (varDefs ctx)

    lookupVar ctx v = HashMap.lookup v (varDefs ctx)

    elemsVar ctx    = HashMap.elems (varDefs ctx)

    addVars ctx vs
        | not $ null (nuVarDefs vs)          = Left $ Error ("Non unique variable definitions: " ++ show (nuVarDefs vs))
        | not $ null (undefinedSorts ctx vs) = Left $ Error ("List of variable definitions with undefined sorts: " ++ show (undefinedSorts ctx vs))
        | otherwise                          = Right $ ctx {varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}

    replaceVars ctx vs
        | not $ null (nuVarDefs vs)          = Left $ Error ("Non unique variable definitions: " ++ show (nuVarDefs vs))
        | not $ null (undefinedSorts ctx vs) = Left $ Error ("List of variable definitions with undefined sorts: " ++ show (undefinedSorts ctx vs))
        | otherwise                          = Right $ ctx {varDefs = toMapByName vs}
