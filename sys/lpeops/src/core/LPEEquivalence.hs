{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEEquivalence
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEEquivalence (
isEquivalentSummand,
isContainedSummand,
removeContainedSummands,
isContainedBySummands,
isContainedByLPE,
areContainedByLPE,
isEquivalentLPE
) where

import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified ValExpr
import qualified VarId
import qualified Constant
import qualified Satisfiability as Sat
import LPETypes
import BlindSubst
import MonadAny

-- Checks if two summands are (definitely) equivalent.
-- This means that the two summands add the exact same behavior to an LPE.
-- It is an under-approximation: two summands can be equivalent even though this function returns false!
isEquivalentSummand :: LPESummand -> LPESummand -> TxsDefs.VExpr -> IOC.IOC Bool
isEquivalentSummand summand1 summand2 invariant =
    -- Both summands must communicate over the same channel:
    if (lpeSmdChan summand1 /= lpeSmdChan summand2) || (lpeSmdPriority summand1 /= lpeSmdPriority summand2)
    then return False
    else do let useChanVars1 = Map.fromList (zipWith (\cv1 cv2 -> (cv2, ValExpr.cstrVar cv1)) (lpeSmdVars summand1) (lpeSmdVars summand2))
            -- Check whether both guards are definitely enabled at the same time with the following expression:
            guard2' <- doBlindSubst useChanVars1 (lpeSmdGuard summand2)
            let guardEq = ValExpr.cstrEqual (lpeSmdGuard summand1) guard2'
            -- Check whether both summands definitely lead to the same next-state with the following expressions:
            procInstEqs <- Monad.mapM (getProcInstEq useChanVars1 (lpeSmdEqs summand2)) (Map.toList (lpeSmdEqs summand1))
            -- Check:
            Sat.isTautology (ValExpr.cstrAnd (Set.fromList (guardEq:procInstEqs))) invariant
-- isEquivalentSummand

-- Checks if the first summand (definitely) 'contains' the second command.
-- This means that the first summand adds behavior to an LPE that is a superset of the behavior that the second summand adds.
-- It is an under-approximation: the first summand can 'contain' the second summand even though this function returns false!
isContainedSummand :: LPESummand -> LPESummand -> TxsDefs.VExpr -> IOC.IOC Bool
isContainedSummand summand1 summand2 invariant =
    -- Both summands must communicate over the same channel:
    if (lpeSmdChan summand1 /= lpeSmdChan summand2) || (lpeSmdPriority summand1 /= lpeSmdPriority summand2)
    then return False
    else do let useChanVars1 = Map.fromList (zipWith (\cv1 cv2 -> (cv2, ValExpr.cstrVar cv1)) (lpeSmdVars summand1) (lpeSmdVars summand2))
            -- Check whether both guards are definitely enabled at the same time with the following expression:
            guard2' <- doBlindSubst useChanVars1 (lpeSmdGuard summand2)
            let guardEq = ValExpr.cstrITE guard2' (lpeSmdGuard summand1) (ValExpr.cstrConst (Constant.Cbool True))
            -- Check whether both summands definitely lead to the same next-state with the following expressions:
            procInstEqs <- Monad.mapM (getProcInstEq useChanVars1 (lpeSmdEqs summand2)) (Map.toList (lpeSmdEqs summand1))
            -- Check:
            Sat.isTautology (ValExpr.cstrAnd (Set.fromList (guardEq:procInstEqs))) invariant
-- isContainedSummand

-- Helper function.
getProcInstEq :: Map.Map VarId.VarId TxsDefs.VExpr -> LPEParamEqs -> (VarId.VarId, TxsDefs.VExpr) -> IOC.IOC TxsDefs.VExpr
getProcInstEq useChanVars1 eqs2 (p1, v1) = do
    let v2 = eqs2 Map.! p1
    v2' <- doBlindSubst useChanVars1 v2
    return (ValExpr.cstrEqual v1 v2')
-- getProcInstEq

-- Only leaves summands in the set that are not 'contained' by another summand in the same set.
removeContainedSummands :: Set.Set LPESummand -> TxsDefs.VExpr -> IOC.IOC (Set.Set LPESummand)
removeContainedSummands summands invariant = Set.fromList <$> Monad.foldM addSummandIfNotContained [] (Set.toList summands)
  where
    addSummandIfNotContained :: [LPESummand] -> LPESummand -> IOC.IOC [LPESummand]
    addSummandIfNotContained [] summand = return [summand]
    addSummandIfNotContained (current:unchecked) summand = do
        summandContainsCurrent <- isContainedSummand summand current invariant
        if summandContainsCurrent
        then do rest <- addSummandIfNotContained unchecked summand
                return (summand:rest) --Summand may be added multiple times; not a problem, later on we create a set anyway
        else do currentContainsSummand <- isContainedSummand current summand invariant
                if currentContainsSummand
                then return (current:unchecked)
                else do rest <- addSummandIfNotContained unchecked summand
                        return (current:rest)
-- removeContainedSummands

isContainedBySummands :: [LPESummand] -> LPESummand -> TxsDefs.VExpr -> IOC.IOC Bool
isContainedBySummands summands summand invariant = anyM (\c -> isContainedSummand c summand invariant) summands

isContainedByLPE :: LPE -> LPESummand -> TxsDefs.VExpr -> IOC.IOC Bool
isContainedByLPE lpe = isContainedBySummands (Set.toList (lpeSummands lpe))

areContainedByLPE :: LPE -> LPESummands -> TxsDefs.VExpr -> IOC.IOC Bool
areContainedByLPE lpe summands invariant = allM (\s -> isContainedByLPE lpe s invariant) (Set.toList summands)

isEquivalentLPE :: LPE -> LPE -> TxsDefs.VExpr -> IOC.IOC Bool
isEquivalentLPE lpe1 lpe2 invariant =
    andM (areContainedByLPE lpe1 (lpeSummands lpe2) invariant) (areContainedByLPE lpe2 (lpeSummands lpe1) invariant)
-- isEquivalentLPE


