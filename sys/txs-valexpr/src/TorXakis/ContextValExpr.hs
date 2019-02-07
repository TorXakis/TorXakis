{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ContextValExpr
( -- * Context
  -- ** instance of ValExpr Context
  ContextValExpr
, fromVarContext
, fromFuncContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import           Data.Either
import qualified Data.HashMap    as HashMap
import qualified Data.Map        as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.FreeVars
import           TorXakis.FuncContext
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort (Sort, HasSort, getSort, memberSort, SortContext(..))
import           TorXakis.Subst
import           TorXakis.ValExpr.Unsafe
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.VarContext (VarContext(..))
import           TorXakis.VarDef
import           TorXakis.VarsDecl

-- | A minimal instance of 'ContextValExpr'.
data ContextValExpr a = ContextValExpr { funcContext :: a
                                         -- var definitions
                                       , varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                       } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create ContextValExpr from FuncContext
fromFuncContext :: a -> ContextValExpr a
fromFuncContext fc = ContextValExpr fc HashMap.empty

-- | Create ContextValExpr from VarContext
fromVarContext :: VarContext a => a -> ContextValExpr (ContextFunc a)
fromVarContext vc = ContextValExpr (fromSortContext vc) (varDefs vc)

instance SortReadContext a => SortReadContext (ContextValExpr a) where
    memberSort = memberSort . funcContext

    memberADT = memberADT . funcContext

    lookupADT = lookupADT . funcContext

    elemsADT = elemsADT . funcContext

instance SortContext a => SortContext (ContextValExpr a) where
    empty = fromFuncContext empty
    addADTs ctx as = case addADTs (funcContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {funcContext = sctx}

instance SortReadContext a => FuncSignatureReadContext (ContextValExpr a) where
    memberFunc = memberFunc . funcContext

    funcSignatures = funcSignatures . funcContext

instance FuncReadContext a => FuncReadContext (ContextValExpr a) where
    lookupFunc = lookupFunc . funcContext

    elemsFunc = elemsFunc . funcContext

instance FuncContext a => FuncContext (ContextValExpr a) where
    addFuncs ctx fds = case addFuncs (funcContext ctx) fds of
                          Left e     -> Left e
                          Right fctx -> Right $ ctx {funcContext = fctx}

instance SortReadContext a => VarReadContext (ContextValExpr a) where
    memberVar ctx v = HashMap.member v (varDefs ctx)

    lookupVar ctx v = HashMap.lookup v (varDefs ctx)

    elemsVar ctx    = HashMap.elems (varDefs ctx)

instance SortContext a => VarContext (ContextValExpr a) where
    addVars ctx vs
        | not $ null nuVarDefs               = Left $ Error ("Non unique variable definitions: " ++ show nuVarDefs)
        | not $ null undefinedSorts          = Left $ Error ("List of variable definitions with undefined sorts: " ++ show undefinedSorts)
        | otherwise                          = Right $ ctx {varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . sort) vs

instance FuncReadContext a => ValExprReadContext (ContextValExpr a)

instance FuncContext a => ValExprContext (ContextValExpr a)