{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextValExprBasis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions.
-----------------------------------------------------------------------------
module TorXakis.ContextValExpr.ContextValExprBasis
( empty
, fromVarContext
)
where
import           TorXakis.ContextValExpr.ContextValExpr
import qualified TorXakis.ContextFunc
import qualified TorXakis.NameMap

-- | empty
empty :: ContextValExpr
empty = fromFuncContext TorXakis.ContextFunc.empty

-- | Create ContextValExpr from VarContext
fromVarContext :: VarContext c => c -> ContextValExpr
fromVarContext vc = ContextValExpr (TorXakis.ContextFunc.fromSortContext vc) (TorXakis.NameMap.toNameMap (elemsVar vc))
