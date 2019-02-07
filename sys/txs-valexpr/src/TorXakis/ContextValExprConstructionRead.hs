{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExprConstructionRead
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions Read only access.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ContextValExprConstructionRead
( -- * Context
  -- ** instance of ValExpr Context
  ContextValExprConstructionRead
, fromFuncSignatureReadContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap    as HashMap
import           GHC.Generics           (Generic)

import           TorXakis.FuncSignatureContext
import           TorXakis.Name
import           TorXakis.Sort (memberSort, SortReadContext(..))
import           TorXakis.ValExprConstructionContext
import           TorXakis.VarDef
import           TorXakis.VarsDecl

-- | A minimal instance of 'ContextValExprConstruction'.
data ContextValExprConstructionRead a = ContextValExprConstructionRead { funcSignatureReadContext :: a
                                                                         -- var definitions
                                                                       , varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                                                       } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create ContextValExprConstruction from FuncSignatureContext
fromFuncSignatureReadContext :: a -> VarsDecl -> ContextValExprConstructionRead a
fromFuncSignatureReadContext fc vs = ContextValExprConstructionRead fc (toMapByName (toList vs))

instance SortReadContext a => SortReadContext (ContextValExprConstructionRead a) where
    memberSort = memberSort . funcSignatureReadContext

    memberADT = memberADT . funcSignatureReadContext

    lookupADT = lookupADT . funcSignatureReadContext

    elemsADT = elemsADT . funcSignatureReadContext

instance FuncSignatureReadContext a => FuncSignatureReadContext (ContextValExprConstructionRead a) where
    memberFunc = memberFunc . funcSignatureReadContext

    funcSignatures = funcSignatures . funcSignatureReadContext

instance SortReadContext a => VarReadContext (ContextValExprConstructionRead a) where
    memberVar ctx v = HashMap.member v (varDefs ctx)

    lookupVar ctx v = HashMap.lookup v (varDefs ctx)

    elemsVar ctx    = HashMap.elems (varDefs ctx)

instance FuncSignatureReadContext a => ValExprConstructionReadContext (ContextValExprConstructionRead a)