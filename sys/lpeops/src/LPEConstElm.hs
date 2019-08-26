{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEConstElm
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module LPEConstElm (
constElm
) where

import qualified Control.Monad       as Monad
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified EnvCore             as IOC
import qualified EnvData
import qualified TxsDefs
import qualified Satisfiability as Sat
import qualified VarId
import qualified ValExpr
import qualified Constant
import           LPETypes
import           LPEParRemoval
import           BlindSubst
import           UntilFixedPoint

-- LPE rewrite method.
-- Eliminates parameters that always have the same value from an LPE.
constElm :: LPEOperation
constElm lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<cstelm>>" ]
    constParams <- untilFixedPointM (getConstParams lpe invariant) (Map.keysSet (lpeInitEqs lpe))
    newLpe <- removeParsFromLPE constParams lpe
    return (Right newLpe)
-- constElm

getConstParams :: LPE -> TxsDefs.VExpr -> Set.Set VarId.VarId -> IOC.IOC (Set.Set VarId.VarId)
getConstParams lpe invariant constParams = do
    let subst = Map.restrictKeys (lpeInitEqs lpe) constParams
    filteredConstParams <- Monad.mapM (filterConstParamsWithSummand subst invariant constParams) (Set.toList (lpeSummands lpe))
    return (foldl Set.intersection constParams filteredConstParams)
-- getConstParams

filterConstParamsWithSummand :: Map.Map VarId.VarId TxsDefs.VExpr -> TxsDefs.VExpr -> Set.Set VarId.VarId -> LPESummand -> IOC.IOC (Set.Set VarId.VarId)
filterConstParamsWithSummand subst invariant constParams summand = do
    filteredConstParams <- Monad.filterM (isConstParamForSummand subst invariant summand) (Set.toList constParams)
    return (Set.fromList filteredConstParams)
-- filterConstParamsWithSummand

isConstParamForSummand :: Map.Map VarId.VarId TxsDefs.VExpr -> TxsDefs.VExpr -> LPESummand -> VarId.VarId -> IOC.IOC Bool
isConstParamForSummand subst invariant summand testParam = do
    let eqExpr = ValExpr.cstrEqual (lpeSmdEqs summand Map.! testParam) (ValExpr.cstrVar testParam)
    let expr = ValExpr.cstrITE (lpeSmdGuard summand) eqExpr (ValExpr.cstrConst (Constant.Cbool True))
    substExpr <- doBlindSubst subst expr
    Sat.isTautology substExpr invariant
-- isConstParamsForSummand


