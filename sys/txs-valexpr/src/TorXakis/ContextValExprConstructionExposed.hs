{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExprConstructionExposed
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions for construction.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module TorXakis.ContextValExprConstructionExposed
( -- * Context
  -- ** instance of ValExpr Context for construction
  ContextValExprConstructionExposed
, fromVarContext
, toVarContext
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
import           TorXakis.ValExprConstructionContext

-- TODO: how is this context used? If always add Variables to a funcSignatureContext then the implementation should be swapped!

-- | An instance of 'TorXakis.ValExprConstructionContext'.
data ContextValExprConstructionExposed a = ContextValExprConstructionExposed { toVarContext :: a
                                                                               -- variable definitions
                                                                             , toFuncSignatures :: Set.Set FuncSignature
                                                                             } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Constructor from VarContext
fromVarContext :: a -> ContextValExprConstructionExposed a
fromVarContext ctx = ContextValExprConstructionExposed ctx Set.empty

-- | Constructor from FuncSignatureContext
fromFuncSignatureContext :: FuncSignatureContext a => a -> ContextValExprConstructionExposed ContextVar
fromFuncSignatureContext ctx = ContextValExprConstructionExposed (fromSortContext ctx) (Set.fromList (funcSignatures ctx))

instance SortContext a => SortContext (ContextValExprConstructionExposed a) where
    memberSort r = memberSort r . toVarContext

    memberADT r = memberADT r . toVarContext

    lookupADT r = lookupADT r . toVarContext

    elemsADT = elemsADT . toVarContext

    addADTs as ctx = case addADTs as (toVarContext ctx) of
                          Left e     -> Left e
                          Right sctx -> Right $ ContextValExprConstructionExposed sctx (toFuncSignatures ctx)

instance VarContext a => VarContext (ContextValExprConstructionExposed a) where
    memberVar v = memberVar v . toVarContext

    lookupVar v = lookupVar v . toVarContext

    elemsVar = elemsVar . toVarContext

    addVars vs ctx = case addVars vs (toVarContext ctx) of
                          Left e     -> Left e
                          Right sctx -> Right $ ContextValExprConstructionExposed sctx (toFuncSignatures ctx)

instance SortContext a => FuncSignatureContext (ContextValExprConstructionExposed a) where
    memberFunc f ctx = Set.member f (toFuncSignatures ctx)

    funcSignatures ctx    = Set.toList (toFuncSignatures ctx)

instance SortContext a => FuncSignatureModifyContext (ContextValExprConstructionExposed a) (ContextValExprConstructionExposed a) where
    addFuncSignatures fs ctx
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ContextValExprConstructionExposed (toVarContext ctx) (Set.union (Set.fromList fs) (toFuncSignatures ctx))
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . flip memberSort ctx) (returnSort f: args f) ) fs

instance VarContext a => ValExprConstructionContext (ContextValExprConstructionExposed a)
