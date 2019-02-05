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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.VarContext
( -- * Context
  -- ** Variable Context class
  VarContext (..)
  -- ** Context Variable instance
, ContextVar
, fromSortContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap    as HashMap
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort ( SortReadContext(..)
                               , SortContext(..)
                               )
import           TorXakis.VarDef

------------------------------------------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------------------------------------------

-- | A Variable Context instance contains all definitions to work with sort and variables
class SortReadContext a => VarReadContext a where
    -- | Accessor for defined variables
    varDefs :: a -> HashMap.Map (RefByName VarDef) VarDef

class SortContext a => VarContext a where
    -- | Add variables to variable context.
    --   A variable context is returned when the following constraints are satisfied:
    --
    --   * The names of the added variables are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    -- Note: added variables might hide previously defined variables.
    -- Note: the order of the variables is not relevant.
    addVarDefs :: a -> [VarDef] -> Either Error a

    -- TODO? addVarsDecl function -> don't need to check uniqueness of added variables.

-- | An instance of 'VarContext'.
data ContextVar a = ContextVar { sortContext :: a
                                 -- variable definitions
                               , _varDefs :: HashMap.Map (RefByName VarDef) VarDef
                               } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create VarContext from SortContext
fromSortContext :: a -> ContextVar a
fromSortContext srt = ContextVar srt HashMap.empty

instance SortReadContext a => SortReadContext (ContextVar a) where
    adtDefs ctx    = adtDefs (sortContext ctx)

instance SortContext a => SortContext (ContextVar a) where
    empty = ContextVar empty HashMap.empty
    addAdtDefs ctx as = case addAdtDefs (sortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {sortContext = sctx}

instance SortReadContext a => VarReadContext (ContextVar a) where
    varDefs = _varDefs

instance SortContext a => VarContext (ContextVar a) where
    addVarDefs ctx vs
        | not $ null nuVarDefs               = Left $ Error ("Non unique variable definitions: " ++ show nuVarDefs)
        | not $ null undefinedSorts          = Left $ Error ("List of variable definitions with undefined sorts: " ++ show undefinedSorts)
        | otherwise                          = Right $ ctx { _varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . elemSort ctx . sort) vs
