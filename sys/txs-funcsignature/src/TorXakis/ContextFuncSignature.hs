{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextFuncSignature
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
{-# LANGUAGE MultiParamTypeClasses     #-}
module TorXakis.ContextFuncSignature
( -- * Context FuncSignature instance
  ContextFuncSignature
, TorXakis.ContextFuncSignature.empty
, fromSortContext
)
where
import qualified Data.Set    as Set

import           TorXakis.ContextSort
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext

-- | An instance of 'TorXakis.FuncSignatureContext'.
data ContextFuncSignature = forall c . SortContext c => 
                            ContextFuncSignature { _sortContext :: c -- not used due to compiler
                                                   -- defined funcSignatures
                                                 , localFuncSignatures :: Set.Set FuncSignature
                                                 }

-- | empty
empty :: ContextFuncSignature
empty = fromSortContext TorXakis.ContextSort.empty

-- | Constructor from SortContext
fromSortContext :: SortContext c => c -> ContextFuncSignature
fromSortContext ctx = ContextFuncSignature ctx Set.empty

instance SortContext ContextFuncSignature where
    -- Can't use
    -- memberSort r = memberSort r . sortContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextFuncSignature ctx _) = memberSort r ctx

    memberADT r (ContextFuncSignature ctx _) = memberADT r ctx

    lookupADT r (ContextFuncSignature ctx _) = lookupADT r ctx

    elemsADT (ContextFuncSignature ctx _) = elemsADT ctx

    addADTs as (ContextFuncSignature ctx vs) = case addADTs as ctx of
                                                Left e     -> Left e
                                                Right sctx -> Right $ ContextFuncSignature sctx vs

instance FuncSignatureContext ContextFuncSignature where
    memberFunc v ctx = Set.member v (localFuncSignatures ctx)

    funcSignatures ctx = Set.toList (localFuncSignatures ctx)

instance FuncSignatureModifyContext ContextFuncSignature ContextFuncSignature where
    addFuncSignatures fs ctx
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ctx {localFuncSignatures = Set.union (Set.fromList fs) (localFuncSignatures ctx)}
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . flip memberSort ctx) (returnSort f: args f) ) fs
