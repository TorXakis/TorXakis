{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextFunc
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Functions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ContextFunc
( -- * Context Function instance
  ContextFunc
, fromSortContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap           as HashMap
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Sort          ( SortReadContext(..)
                                        , SortContext(..)
                                        )
import           TorXakis.FuncContext
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext

-- | An instance of 'TorXakis.FuncContext'.
data ContextFunc a = ContextFunc { sortContext :: a
                                   -- funcDefs
                                 , funcDefs :: HashMap.Map FuncSignature FuncDef
                                                   } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create VarContext from SortContext
fromSortContext :: a -> ContextFunc a
fromSortContext srt = ContextFunc srt HashMap.empty

instance SortReadContext a => SortReadContext (ContextFunc a) where
    memberSort   = memberSort . sortContext

    memberADT = memberADT . sortContext

    lookupADT = lookupADT . sortContext

    elemsADT  = elemsADT . sortContext

instance SortContext a => SortContext (ContextFunc a) where
    empty = fromSortContext empty
    addADTs ctx as = case addADTs (sortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {sortContext = sctx}

instance SortReadContext a => FuncSignatureReadContext (ContextFunc a) where
    memberFunc ctx v   = HashMap.member v (funcDefs ctx)

    funcSignatures ctx = HashMap.keys (funcDefs ctx)

instance SortReadContext a => FuncReadContext (ContextFunc a) where
    lookupFunc ctx v = HashMap.lookup v (funcDefs ctx)

    elemsFunc ctx    = HashMap.elems (funcDefs ctx)

instance SortContext a => FuncContext (ContextFunc a) where
    addFuncs ctx fs
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
                                    -- TODO: check more!
        | otherwise                          = Right $ ctx {funcDefs = HashMap.union (toMapByFuncSignature ctx fs) (funcDefs ctx)}
      where
        nuFuncSignatures :: [FuncDef]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncDef]
        undefinedSorts = filter (\f -> any (not . memberSort ctx) (returnSort f: args f) ) fs
