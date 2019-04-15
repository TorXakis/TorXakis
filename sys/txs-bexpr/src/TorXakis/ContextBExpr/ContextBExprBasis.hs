{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextBExprBasis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Behaviour Expressions.
-----------------------------------------------------------------------------
module TorXakis.ContextBExpr.ContextBExprBasis
( empty
)
where
import           TorXakis.ContextBExpr.ContextBExpr
import qualified TorXakis.ContextProc

-- | empty
empty :: ContextBExpr
empty = fromProcContext TorXakis.ContextProc.empty