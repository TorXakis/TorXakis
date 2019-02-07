{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExprConstruction
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
module TorXakis.ContextValExprConstruction
( -- * Context
  -- ** instance of ValExpr Context
  ContextValExprConstruction
, fromVarContext
, fromFuncSignatureContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap    as HashMap
import           GHC.Generics           (Generic)

import           TorXakis.ContextFuncSignature
import           TorXakis.Error
import           TorXakis.FuncSignatureContext
import           TorXakis.Name
import           TorXakis.Sort (memberSort, SortContext(..), SortReadContext(..))
import           TorXakis.ValExprConstructionContext
import           TorXakis.VarContext (VarContext(..))
import           TorXakis.VarDef

-- | A minimal instance of 'ContextValExprConstruction'.
data ContextValExprConstruction a = ContextValExprConstruction { funcSignatureContext :: a
                                                                 -- var definitions
                                                               , varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                                               } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create ContextValExprConstruction from FuncSignatureContext
fromFuncSignatureContext :: a -> ContextValExprConstruction a
fromFuncSignatureContext fc = ContextValExprConstruction fc HashMap.empty

-- | Create ContextValExprConstruction from VarContext
fromVarContext :: VarContext a => a -> ContextValExprConstruction (ContextFuncSignature a)
fromVarContext vc = ContextValExprConstruction (fromSortContext vc) (toMapByName (elemsVar vc))

instance SortReadContext a => SortReadContext (ContextValExprConstruction a) where
    memberSort = memberSort . funcSignatureContext

    memberADT = memberADT . funcSignatureContext

    lookupADT = lookupADT . funcSignatureContext

    elemsADT = elemsADT . funcSignatureContext

instance SortContext a => SortContext (ContextValExprConstruction a) where
    empty = fromFuncSignatureContext empty
    addADTs ctx as = case addADTs (funcSignatureContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {funcSignatureContext = sctx}

instance FuncSignatureReadContext a => FuncSignatureReadContext (ContextValExprConstruction a) where
    memberFunc = memberFunc . funcSignatureContext

    funcSignatures = funcSignatures . funcSignatureContext

instance FuncSignatureContext a => FuncSignatureContext (ContextValExprConstruction a) where
    addFuncSignatures ctx fs = case addFuncSignatures (funcSignatureContext ctx) fs of
                                  Left e     -> Left e
                                  Right fctx -> Right $ ctx {funcSignatureContext = fctx}

instance SortReadContext a => VarReadContext (ContextValExprConstruction a) where
    memberVar ctx v = HashMap.member v (varDefs ctx)

    lookupVar ctx v = HashMap.lookup v (varDefs ctx)

    elemsVar ctx    = HashMap.elems (varDefs ctx)

instance SortContext a => VarContext (ContextValExprConstruction a) where
    addVars ctx vs
        | not $ null nuVarDefs               = Left $ Error ("Non unique variable definitions: " ++ show nuVarDefs)
        | not $ null undefinedSorts          = Left $ Error ("List of variable definitions with undefined sorts: " ++ show undefinedSorts)
        | otherwise                          = Right $ ctx {varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . sort) vs

instance FuncSignatureReadContext a => ValExprConstructionReadContext (ContextValExprConstruction a)

instance FuncSignatureContext a => ValExprConstructionContext (ContextValExprConstruction a)