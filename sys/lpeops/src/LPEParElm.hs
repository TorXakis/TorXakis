{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEParElm
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEParElm (
parElm
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified EnvData
import qualified FreeVar
import qualified VarId
import LPETypes
import LPEParRemoval
import UntilFixedPoint

-- Eliminates inert parameters (=parameters that do not contribute to the behavior of a process) from an LPE:
parElm :: LPEOperation
parElm lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<parelm>>" ]
    let allParams = Set.fromList (Map.keys (lpeInitEqs lpe))
    let guardParams = Set.fromList (concatMap (FreeVar.freeVars . lpeSmdGuard) (lpeSmdList lpe))
    -- All parameters are initially assumed to be inert, except those used in a guard.
    -- This initial set of inert parameters is reduced until a fixpoint is reached:
    let inertParams = untilFixedPoint (getInertParams (lpeSmdList lpe)) (allParams Set.\\ guardParams)
    -- The remaining inert parameters are removed from the LPE:
    newLpe <- removeParsFromLPE inertParams lpe
    return (Right newLpe)
-- parElm

-- Removes from the set of inert parameter all variables (=superset of parameters) that are assigned to parameters that are NOT inert:
getInertParams :: [LPESummand] -> Set.Set VarId.VarId -> Set.Set VarId.VarId
getInertParams summands inertParams =
    inertParams Set.\\ Set.unions (map getParamsAssignedToNonInertParams summands)
  where
    getParamsAssignedToNonInertParams :: LPESummand -> Set.Set VarId.VarId
    getParamsAssignedToNonInertParams summand =
        let nonInertParamAssignExprs = Map.elems (Map.withoutKeys (lpeSmdEqs summand) inertParams) in
          Set.fromList (concatMap FreeVar.freeVars nonInertParamAssignExprs)
-- getInertParams

