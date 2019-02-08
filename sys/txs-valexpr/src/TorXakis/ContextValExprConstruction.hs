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
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.ContextVar
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext
import           TorXakis.Sort (memberSort, SortContext(..))
import           TorXakis.ValExprConstructionContext
import           TorXakis.VarContext (VarContext(..))

-- | An instance of 'TorXakis.ValExprConstructionContext'.
data ContextValExprConstruction a = ContextValExprConstruction { varContext :: a
                                                                 -- funcSignatures
                                                               , _funcSignatures :: Set.Set FuncSignature
                                                               } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create ContextValExprConstruction from FuncSignatureContext
fromFuncSignatureContext :: FuncSignatureContext a => a -> ContextValExprConstruction (ContextVar a)
fromFuncSignatureContext fc = ContextValExprConstruction (fromSortContext fc) (Set.fromList (funcSignatures fc))

-- | Create ContextValExprConstruction from VarContext
fromVarContext :: a -> ContextValExprConstruction a
fromVarContext vc = ContextValExprConstruction vc Set.empty

instance SortContext a => SortContext (ContextValExprConstruction a) where
    empty = fromVarContext empty

    memberSort = memberSort . varContext

    memberADT = memberADT . varContext

    lookupADT = lookupADT . varContext

    elemsADT = elemsADT . varContext

    addADTs ctx as = case addADTs (varContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {varContext = sctx}

instance VarContext a => VarContext (ContextValExprConstruction a) where
    memberVar = memberVar . varContext

    lookupVar = lookupVar . varContext

    elemsVar = elemsVar . varContext

    addVars ctx vs = case addVars (varContext ctx) vs of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {varContext = sctx}

    replaceVars ctx vs = case replaceVars (varContext ctx) vs of
                              Left e     -> Left e
                              Right sctx -> Right $ ctx {varContext = sctx}

instance VarContext a => FuncSignatureContext (ContextValExprConstruction a) where
    memberFunc ctx v = Set.member v (_funcSignatures ctx)

    funcSignatures ctx    = Set.toList (_funcSignatures ctx)

instance VarContext a => FuncSignatureModifyContext (ContextValExprConstruction a) (ContextValExprConstruction a) where
    addFuncSignatures ctx fs
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ctx {_funcSignatures = Set.union (Set.fromList fs) (_funcSignatures ctx)}
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . memberSort ctx) (returnSort f: args f) ) fs

instance VarContext a => ValExprConstructionContext (ContextValExprConstruction a)
