{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for BExpr: all defined sorts, functions, processes, variables, and channels
-----------------------------------------------------------------------------
module TorXakis.BExprContext
( -- * BExpr Context
  BExprContext
  -- dependencies, yet part of interface
, module TorXakis.ChanContext
, module TorXakis.ProcContext
, module TorXakis.VarContext
, module TorXakis.ValExprContext
)
where
import           TorXakis.ChanContext
import           TorXakis.ProcContext
import           TorXakis.ValExprContext
import           TorXakis.VarContext

-- | A BExprContext instance contains all definitions to create behavioural expressions
class (VarContext c, ProcContext c, ChanContext c, ValExprContext c) => BExprContext c      -- TODO: can this be nicer ProcContext => FuncContext
                                                                                            --                         VarContext + FuncContext <==> ValExprContext