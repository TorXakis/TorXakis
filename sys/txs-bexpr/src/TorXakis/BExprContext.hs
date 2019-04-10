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
  BExprContext(..)
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap           as HashMap
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           TorXakis.BExpr.BExpr
import           TorXakis.Chan
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.ProcDef
import           TorXakis.ProcExit
import           TorXakis.ProcSignature
import           TorXakis.Sort          ( Sort, SortContext (..), memberSort )
import           TorXakis.ValExprContext
import           TorXakis.VarContext
import           TorXakis.Var

-- | A BExprContext instance contains all definitions to create behavioural expressions
class (VarContext c, ProcContext c, ChanContext c) => BExprContext c
