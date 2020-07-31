{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEClean
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEClean (
cleanLPE
) where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified EnvData
import qualified Satisfiability as Sat
import LPETypes
import LPESuccessors
import LPEEquivalence
import BlindSubst
import UntilFixedPoint

-- Removes superfluous summands, e.g. summands that do not add new behavior to the LPE.
-- Also removes summands that are unreachable from the initial state and unreachable from the next-states of all other summands.
-- (Basically, we do a partial, symbolic reachability analysis.)
cleanLPE :: LPEOperation
cleanLPE lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<clean>>" ]
    let summands = lpeSummands lpe
    uniqueSummands <- removeContainedSummands summands invariant
    Monad.when (length uniqueSummands < Set.size summands) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Removed " ++ show (Set.size summands - length uniqueSummands) ++ " superfluous summands") ])
    initReachableSummands <- Monad.foldM addSummandIfReachableFromInit Set.empty uniqueSummands
    reachableSummands <- untilFixedPointM (updateReachableSummands uniqueSummands) initReachableSummands
    Monad.when (length reachableSummands < length uniqueSummands) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Removed " ++ show (length uniqueSummands - length reachableSummands) ++ " unreachable summands") ])
    --let unreachableSummands = Set.map (\s -> s { lpeSmdDebug = "UNREACHABLE" }) (uniqueSummands Set.\\ reachableSummands)
    --return (Right (lpe { lpeSummands = Set.union reachableSummands unreachableSummands }))
    return (Right (lpe { lpeSummands = reachableSummands }))
  where
    addSummandIfReachableFromInit :: Set.Set LPESummand -> LPESummand -> IOC.IOC (Set.Set LPESummand)
    addSummandIfReachableFromInit soFar candidate = do
        -- Check if the summand can be reached via the initial state:
        guard' <- doBlindSubst (lpeInitEqs lpe) (lpeSmdGuard candidate)
        sat <- Sat.couldBeSatisfiable guard' invariant
        if sat
        then return (Set.insert candidate soFar)
        else return soFar
    -- addSummandIfReachableFromInit
    
    updateReachableSummands :: Set.Set LPESummand -> Set.Set LPESummand -> IOC.IOC (Set.Set LPESummand)
    updateReachableSummands uniqueSummands reachableSummands = do
        IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Reached " ++ show (Set.size reachableSummands) ++ " of " ++ show (Set.size uniqueSummands) ++ " summands") ]
        newSummands <- Set.fromList <$> Monad.filterM (isReachableFrom reachableSummands) (Set.toList (uniqueSummands Set.\\ reachableSummands))
        return (Set.union reachableSummands newSummands)
    -- updateReachableSummands
    
    isReachableFrom :: Set.Set LPESummand -> LPESummand -> IOC.IOC Bool
    isReachableFrom reachableSummands summand = do
        predecessors <- getPossiblePredecessors reachableSummands invariant summand
        return (Set.delete summand (Set.fromList predecessors) /= Set.empty)
-- cleanLPE

