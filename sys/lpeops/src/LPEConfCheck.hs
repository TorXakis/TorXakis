{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEConfCheck
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEConfCheck (
confCheck,
confElm
) where

import qualified Control.Monad       as Monad
import qualified Data.Set            as Set
import qualified EnvCore             as IOC
import qualified EnvData
import qualified VarId
import qualified TxsDefs
--import qualified TxsShow
import qualified Satisfiability as Sat
import           LPETypes
import qualified ValExpr
import qualified Constant
import           LPESuccessors
import           LPEBlindSubst
import           LPEEquivalence
import           MonadAny

-- LPE rewrite method.
-- Flags confluent ISTEPs by setting their Prioritized flag to True
confCheck :: LPEOperation
confCheck lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<confcheck>>" ]
    confluentTauSummands <- filterConfluentTauSummands (lpeSmdList lpe) invariant
    let nonConfluentTauSummands = lpeSummands lpe Set.\\ confluentTauSummands
    let newSummands = Set.union nonConfluentTauSummands (Set.map flagSummand confluentTauSummands)
    return (Right (lpe { lpeSummands = newSummands }))
  where
    flagSummand :: LPESummand -> LPESummand
    flagSummand summand = summand { lpeSmdPriority = True }
-- confCheck

filterConfluentTauSummands :: [LPESummand] -> TxsDefs.VExpr -> IOC.IOC LPESummands
filterConfluentTauSummands summands invariant = do
    let tauSummands = filter (\s -> lpeSmdChan s == TxsDefs.chanIdIstep) summands
    confluentTauSummands <- Monad.filterM isConfluentSummand tauSummands
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Detected " ++ show (length confluentTauSummands) ++ " confluent ISTEP summand(s) out of " ++ show (length tauSummands) ++ "!") ]
    return (Set.fromList confluentTauSummands)
  where
    isConfluentSummand :: LPESummand -> IOC.IOC Bool
    isConfluentSummand tauSummand = allM (checkConfluenceCondition invariant tauSummand) summands
-- filterConfluentTauSummands

checkConfluenceCondition :: TxsDefs.VExpr -> LPESummand -> LPESummand -> IOC.IOC Bool
checkConfluenceCondition invariant summand1 summand2 = do
    -- Summands are always confluent with themselves and equivalent summands:
    equiv <- isEquivalentSummand summand1 summand2 invariant
    if equiv
    then return True
    else do -- g1 && g2
            -- "Given that the two summands are both enabled, it must follow that ..."
            let premise = ValExpr.cstrAnd (Set.fromList [lpeSmdGuard summand1, lpeSmdGuard summand2])
            
            -- g1[s2] && g2[s1] && ...
            -- "... (1/4) a summand must be enabled after the other has been applied"
            g1 <- doBlindSubst (lpeSmdEqs summand2) (lpeSmdGuard summand1)
            g2 <- doBlindSubst (lpeSmdEqs summand1) (lpeSmdGuard summand2)
            
            -- a1 == a1[s2] && ... && an == an[s2]
            -- "... (2/4) the action arguments of the first summand must be the same before and after the application of the second summand"
            channelArgEqs1 <- Monad.mapM (getChannelArgEq (lpeSmdEqs summand1)) (lpeSmdVars summand2)
            
            -- b1 == b1[s1] && ... && bn == bn[s1]
            -- "... (3/4) the action arguments of the second summand must be the same before and after the application of the first summand"
            channelArgEqs2 <- Monad.mapM (getChannelArgEq (lpeSmdEqs summand2)) (lpeSmdVars summand1)
            
            -- x1[s1][s2] == x1[s2][s1] && ... && xn[s1][s2] == xn[s2][s1]
            -- "... (4/4) we find ourselves in the same state after applying the two summands, regardless of the order"
            instantiationEqs <- Monad.mapM getInstantiationEq (lpeSmdParamList summand1)
            
            -- Combine the different :
            let conclusion = ValExpr.cstrAnd (Set.fromList ([g1, g2] ++ channelArgEqs1 ++ channelArgEqs2 ++ instantiationEqs))
            let confluenceCondition = ValExpr.cstrITE premise conclusion (ValExpr.cstrConst (Constant.Cbool True))
            
            -- Is the confluence condition a tautology?
            Sat.isTautology confluenceCondition invariant
  where
    getChannelArgEq :: LPEParamEqs -> VarId.VarId -> IOC.IOC TxsDefs.VExpr
    getChannelArgEq eqs param = do
        let paramExpr = ValExpr.cstrVar param
        e <- doBlindSubst eqs paramExpr
        return (ValExpr.cstrEqual paramExpr e)
    -- getChannelArgEq
    
    getInstantiationEq :: VarId.VarId -> IOC.IOC TxsDefs.VExpr
    getInstantiationEq param = do
        let paramExpr = ValExpr.cstrVar param
        e1 <- doBlindSubst (lpeSmdEqs summand2) paramExpr
        e1' <- doBlindSubst (lpeSmdEqs summand1) e1
        e2 <- doBlindSubst (lpeSmdEqs summand1) paramExpr
        e2' <- doBlindSubst (lpeSmdEqs summand2) e2
        return (ValExpr.cstrEqual e1' e2')
-- checkConfluenceCondition

-- LPE rewrite method.
-- Appends confluent ISTEPs to predecessor summands.
confElm :: LPEOperation
confElm lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<confelm>>" ]
    confluentTauSummands <- filterConfluentTauSummands (lpeSmdList lpe) invariant
    if confluentTauSummands == Set.empty
    then return $ Right lpe
    else do let orderedSummands = lpeSmdList lpe
            definiteSuccessors <- Monad.mapM (getDefiniteSuccessors (lpeSummands lpe) invariant) orderedSummands
            let confluentTauSuccessors = map (Set.toList . Set.intersection confluentTauSummands . Set.fromList) definiteSuccessors
            mergedSummands <- Monad.mapM mergeZippedSummands (zip orderedSummands confluentTauSuccessors)
            return $ Right (lpe { lpeSummands = Set.fromList mergedSummands })
  where
    mergeZippedSummands :: (LPESummand, [LPESummand]) -> IOC.IOC LPESummand
    mergeZippedSummands (summand, []) = return summand
    mergeZippedSummands (summand1, summand2:_) = do
        newEqs <- doBlindParamEqsSubst (lpeSmdEqs summand1) (lpeSmdEqs summand2)
        return (summand1 { lpeSmdEqs = newEqs, lpeSmdPriority = False })
-- confElm

