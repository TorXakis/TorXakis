{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextVarExposed
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Variables.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.ContextVarExposed
( -- * Context Variable instance
  ContextVarExposed
, fromSortContext
, toSortContext
)
where
import           Data.Data              (Data)
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.SortContext
import           TorXakis.VarContext
import           TorXakis.Var

-- | An instance of 'TorXakis.VarContext'.
data ContextVarExposed a = ContextVarExposed { toSortContext :: a
                                               -- variable definitions
                                             , varDefs :: NameMap VarDef
                                             } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor from SortContext
fromSortContext :: a -> ContextVarExposed a
fromSortContext ctx = ContextVarExposed ctx empty

instance SortContext a => SortContext (ContextVarExposed a) where
    memberSort r = memberSort r . toSortContext

    memberADT r = memberADT r . toSortContext

    lookupADT r = lookupADT r . toSortContext

    elemsADT = elemsADT . toSortContext

    addADTs as ctx = case addADTs as (toSortContext ctx) of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {toSortContext = sctx}

-- | non unique Variable Definitions (i.e. duplicate names)
nuVarDefs :: [VarDef] -> [VarDef]
nuVarDefs = repeatedByName

-- | undefined Sorts of Variable Definitions.
undefinedSorts :: SortContext a => [VarDef] -> a -> [VarDef]
undefinedSorts vs ctx = filter (not . flip memberSort ctx . sort) vs

instance SortContext a => VarContext (ContextVarExposed a) where
    memberVar v ctx = member v (varDefs ctx)

    lookupVar v ctx = TorXakis.NameMap.lookup v (varDefs ctx)

    elemsVar ctx    = elems (varDefs ctx)

    addVars vs ctx
        | not $ null (nuVarDefs vs)          = Left $ Error ("Non unique variable definitions: " ++ show (nuVarDefs vs))
        | not $ null (undefinedSorts vs ctx) = Left $ Error ("List of variable definitions with undefined sorts: " ++ show (undefinedSorts vs ctx))
        | otherwise                          = Right $ ctx {varDefs = toNameMap vs `union` varDefs ctx}
