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
( -- * Context Variable instance
  ContextFuncSignature
, fromSortContext
)
where
import qualified Data.Set    as Set

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext

-- | An instance of 'TorXakis.VarContext'.
data ContextFuncSignature = forall a . SortContext a => 
                            ContextFuncSignature { _sortContext :: a -- not used due to compiler
                                                   -- defined funcSignatures
                                                 , localFuncSignatures :: Set.Set FuncSignature
                                                 }
-- | Constructor from SortContext
fromSortContext :: SortContext b => b -> ContextFuncSignature
fromSortContext ctx = ContextFuncSignature ctx Set.empty

instance SortContext ContextFuncSignature where
    -- Can't use
    -- memberSort   = memberSort . sortContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort (ContextFuncSignature ctx _) = memberSort ctx

    memberADT (ContextFuncSignature ctx _) = memberADT ctx

    lookupADT (ContextFuncSignature ctx _) = lookupADT ctx

    elemsADT (ContextFuncSignature ctx _) = elemsADT ctx

    addADTs (ContextFuncSignature ctx vs) as = case addADTs ctx as of
                                                Left e     -> Left e
                                                Right sctx -> Right $ ContextFuncSignature sctx vs

instance FuncSignatureContext ContextFuncSignature where
    memberFunc ctx v = Set.member v (localFuncSignatures ctx)

    funcSignatures ctx = Set.toList (localFuncSignatures ctx)

instance FuncSignatureModifyContext ContextFuncSignature ContextFuncSignature where
    addFuncSignatures ctx fs
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ctx {localFuncSignatures = Set.union (Set.fromList fs) (localFuncSignatures ctx)}
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . memberSort ctx) (returnSort f: args f) ) fs
