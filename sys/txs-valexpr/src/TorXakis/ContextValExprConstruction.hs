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
-- Context containing Value Expressions for construction.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module TorXakis.ContextValExprConstruction
( -- * Context
  -- ** instance of ValExpr Context for construction
  ContextValExprConstruction
, fromVarContext
, fromFuncSignatureContext
)
where
import qualified Data.Set               as Set

import           TorXakis.ContextVar
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.ValExprConstructionContext

-- TODO: how is this context used? If always add Variables to a funcSignatureContext then the implementation should be swapped!

-- | An instance of 'TorXakis.ValExprConstructionContext'.
data ContextValExprConstruction = forall a . VarContext a => 
                                        ContextValExprConstruction { _varContext :: a -- not used due to compiler
                                                                     -- funcSignatures
                                                                   , localFuncSignatures :: Set.Set FuncSignature
                                                                   }

-- | Constructor from VarContext
fromVarContext :: VarContext b => b -> ContextValExprConstruction
fromVarContext ctx = ContextValExprConstruction ctx Set.empty

-- | Constructor from FuncSignatureContext
fromFuncSignatureContext :: FuncSignatureContext b => b -> ContextValExprConstruction
fromFuncSignatureContext ctx = ContextValExprConstruction (fromSortContext ctx) (Set.fromList (funcSignatures ctx))

instance SortContext ContextValExprConstruction where
    -- Can't use
    -- memberSort   = memberSort . varContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort (ContextValExprConstruction ctx _) = memberSort ctx

    memberADT (ContextValExprConstruction ctx _) = memberADT ctx

    lookupADT (ContextValExprConstruction ctx _) = lookupADT ctx

    elemsADT (ContextValExprConstruction ctx _) = elemsADT ctx

    addADTs (ContextValExprConstruction ctx vs) as = case addADTs ctx as of
                                                        Left e     -> Left e
                                                        Right sctx -> Right $ ContextValExprConstruction sctx vs

instance VarContext ContextValExprConstruction where
    memberVar (ContextValExprConstruction ctx _) = memberVar ctx

    lookupVar (ContextValExprConstruction ctx _) = lookupVar ctx

    elemsVar (ContextValExprConstruction ctx _) = elemsVar ctx

    addVars (ContextValExprConstruction ctx vs) as = case addVars ctx as of
                                                        Left e     -> Left e
                                                        Right sctx -> Right $ ContextValExprConstruction sctx vs

    replaceVars (ContextValExprConstruction ctx vs) as = case replaceVars ctx as of
                                                                Left e     -> Left e
                                                                Right sctx -> Right $ ContextValExprConstruction sctx vs

instance FuncSignatureContext ContextValExprConstruction where
    memberFunc ctx v = Set.member v (localFuncSignatures ctx)

    funcSignatures ctx    = Set.toList (localFuncSignatures ctx)

instance FuncSignatureModifyContext ContextValExprConstruction ContextValExprConstruction where
    addFuncSignatures ctx fs
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ctx {localFuncSignatures = Set.union (Set.fromList fs) (localFuncSignatures ctx)}
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . memberSort ctx) (returnSort f: args f) ) fs

instance ValExprConstructionContext ContextValExprConstruction
