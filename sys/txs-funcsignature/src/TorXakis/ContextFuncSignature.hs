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
-- Context for Function Signatures.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ContextFuncSignature
( -- * Context Variable instance
  ContextFuncSignature
, fromSortContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Sort          ( SortReadContext(..)
                                        , SortContext(..)
                                        )
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext

-- | An instance of 'TorXakis.FuncSignatureContext'.
data ContextFuncSignature a = ContextFuncSignature { sortContext :: a
                                                     -- funcSignatures
                                                   , _funcSignatures :: Set.Set FuncSignature
                                                   } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create VarContext from SortContext
fromSortContext :: a -> ContextFuncSignature a
fromSortContext srt = ContextFuncSignature srt Set.empty

instance SortReadContext a => SortReadContext (ContextFuncSignature a) where
    memberSort   = memberSort . sortContext

    memberADT = memberADT . sortContext

    lookupADT = lookupADT . sortContext

    elemsADT  = elemsADT . sortContext

instance SortContext a => SortContext (ContextFuncSignature a) where
    empty = fromSortContext empty
    addADTs ctx as = case addADTs (sortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {sortContext = sctx}

instance SortReadContext a => FuncSignatureReadContext (ContextFuncSignature a) where
    memberFunc ctx v = Set.member v (_funcSignatures ctx)

    funcSignatures ctx    = Set.toList (_funcSignatures ctx)

instance SortContext a => FuncSignatureContext (ContextFuncSignature a) where
    addFuncSignatures ctx fs
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = Right $ ctx {_funcSignatures = Set.union (Set.fromList fs) (_funcSignatures ctx)}
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . memberSort ctx) (returnSort f: args f) ) fs
