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
                               , localFuncSignatures :: Set.Set FuncSignature
                               } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Constructor from SortContext
fromSortContext :: a -> ContextFuncSignatureExposed a
fromSortContext ctx = ContextFuncSignatureExposed ctx Set.empty

instance SortContext a => SortContext (ContextFuncSignatureExposed a) where
    memberSort   = memberSort . toSortContext

    memberADT = memberADT . toSortContext

    lookupADT = lookupADT . toSortContext

    elemsADT  = elemsADT . toSortContext

    addADTs ctx as = case addADTs (toSortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {toSortContext = sctx}

instance SortContext a => FuncSignatureContext (ContextFuncSignatureExposed a) where
    memberFunc ctx v = Set.member v (localFuncSignatures ctx)

    funcSignatures ctx = Set.toList (localFuncSignatures ctx)

instance SortContext a => FuncSignatureModifyContext (ContextFuncSignatureExposed a) (ContextFuncSignatureExposed a) where
    addFuncSignatures ctx fs
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ctx {localFuncSignatures = Set.union (Set.fromList fs) (localFuncSignatures ctx)}
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . memberSort ctx) (returnSort f: args f) ) fs
