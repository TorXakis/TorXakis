{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module LPEPar
( lpePar )

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import TxsDefs
import qualified TxsUtils
import LPEHelpers
import LPE

import ProcId
import ChanId
import VarId
import BehExprDefs
import SortId
import StdTDefs (stdSortTable)

import qualified Data.Text         as T

import Expand (relabel)
import Subst

import TranslatedProcDefs

import Debug.Trace

type ProcDefs = Map.Map TxsDefs.ProcId TxsDefs.ProcDef

type Proc = (ProcId, [ChanId])
type PCMapping = Map.Map Proc Integer
type ProcToParams = Map.Map Proc [VarId]

type ChanMapping = Map.Map ChanId ChanId
type ParamMapping = Map.Map VarId VExpr

-- ----------------------------------------------------------------------------------------- --
-- LPE :
-- ----------------------------------------------------------------------------------------- --

intSort = case Map.lookup (T.pack "Int") stdSortTable of
                    Just sort   -> sort
                    Nothing     -> error "LPE module: could not find standard IntSort"

--emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
--                                             , TranslatedProcDefs.lGNF = []
--                                             , TranslatedProcDefs.lLPE = [] }


lpePar :: BExpr -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpePar procInst@(ProcInst procIdInst chansInst paramsInst) translatedProcDefs procDefs =
  (procInst, procDefs)
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
