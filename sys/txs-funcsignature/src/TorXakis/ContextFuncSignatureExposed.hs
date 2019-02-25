{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextFuncSignatureExposed
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
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextFuncSignatureExposed
( -- * Context FuncSignature instance
  ContextFuncSignatureExposed
, fromSortContext
, toSortContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext

-- | An instance of 'TorXakis.FuncSignatureContext'.
data ContextFuncSignatureExposed a = ContextFuncSignatureExposed { toSortContext :: a
                                                                   -- variable definitions
                                                                 , toFuncSignatures :: Set.Set FuncSignature
                                                                 } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Constructor from SortContext
fromSortContext :: a -> ContextFuncSignatureExposed a
fromSortContext ctx = ContextFuncSignatureExposed ctx Set.empty

instance SortContext a => SortContext (ContextFuncSignatureExposed a) where
    memberSort r = memberSort r . toSortContext

    memberADT r = memberADT r . toSortContext

    lookupADT r = lookupADT r . toSortContext

    elemsADT  = elemsADT . toSortContext

    addADTs as ctx = case addADTs as (toSortContext ctx) of
                          Left e     -> Left e
                          Right sctx -> Right $ ContextFuncSignatureExposed sctx (toFuncSignatures ctx)

instance SortContext a => FuncSignatureContext (ContextFuncSignatureExposed a) where
    memberFunc v ctx = Set.member v (toFuncSignatures ctx)

    funcSignatures ctx = Set.toList (toFuncSignatures ctx)

instance SortContext a => FuncSignatureModifyContext (ContextFuncSignatureExposed a) (ContextFuncSignatureExposed a) where
    addFuncSignatures fs ctx
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ContextFuncSignatureExposed (toSortContext ctx) (Set.union (Set.fromList fs) (toFuncSignatures ctx))
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs -- TODO: discuss a funcSignature has a reference (identity)?

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . flip memberSort ctx) (returnSort f: args f) ) fs
