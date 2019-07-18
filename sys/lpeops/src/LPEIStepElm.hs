{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEIStepElm
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEIStepElm (
iStepElm
) where

import qualified Data.List           as List
import qualified Control.Monad       as Monad
import qualified Data.Set            as Set
import qualified EnvCore             as IOC
import qualified EnvData
import qualified ValExpr
import LPETypes
import LPEBlindSubst

-- Removes duplicate summands and summands that are unreachable by all other (!) summands
-- (so basically we do a partial, symbolic reachability analysis).
iStepElm :: LPEOperation
iStepElm lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<istepelm>>" ]
    let (iStepSummands, nonIStepSummands) = List.partition (\(LPESummand _ offers _ _) -> null offers) (Set.toList (lpeSummands lpe))
    combinedSummands <- Monad.mapM (combineSummand nonIStepSummands) iStepSummands
    return (Right (lpe { lpeSummands = Set.fromList (nonIStepSummands ++ concat combinedSummands) }))
  where
    combineSummand :: [LPESummand] -> LPESummand -> IOC.IOC [LPESummand]
    combineSummand nonIStepSummand iStepSummand =
        Monad.mapM (combineTwoSummands iStepSummand) nonIStepSummand
    -- combineSummand
    
    combineTwoSummands :: LPESummand -> LPESummand -> IOC.IOC LPESummand
    combineTwoSummands summand1 summand2 = do
        newGuard2 <- doConfidentSubst summand2 (lpeSmdEqs summand1) (lpeSmdGuard summand2)
        let combinedGuard = ValExpr.cstrAnd (Set.fromList [lpeSmdGuard summand1, newGuard2])
        combinedParamEqs <- doConfidentParamEqsSubst summand2 (lpeSmdEqs summand1) (lpeSmdEqs summand2)
        return (LPESummand { lpeSmdVars = lpeSmdVars summand1 `Set.union` lpeSmdVars summand2
                           , lpeSmdOffers = lpeSmdOffers summand2
                           , lpeSmdGuard = combinedGuard
                           , lpeSmdEqs = combinedParamEqs
                           })
-- iStepElm

