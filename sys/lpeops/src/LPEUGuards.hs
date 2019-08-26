{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEUGuards
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEUGuards (
addUGuardsToLPE
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified EnvData
import qualified TxsDefs
import qualified ChanId
import qualified VarId
import qualified ValExpr
import qualified Constant
import qualified Satisfiability as Sat
import Pairs
import LPETypes
import LPEBlindSubst
import LPESuccessors
import LPEDeterminism
import UntilFixedPoint
import VarFactory

-- Makes the LPE deterministic by delaying non-deterministic choices by one step until a fixpoint is reached.
addUGuardsToLPE :: LPEOperation
addUGuardsToLPE lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<uguard>>" ]
    newLpe <- untilCounterOrFixedPointM 1 (doIteration invariant) lpe
    return (Right newLpe)
-- addUGuardsToLPE

doIteration :: TxsDefs.VExpr -> LPE -> IOC.IOC LPE
doIteration invariant lpe = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Looking for same-state summand pairs in " ++ show (Set.size (lpeSummands lpe)) ++ " summands...") ]
    sameStatePairs <- getSameStateSummandPairs invariant (Set.toList (lpeSummands lpe))
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Checking " ++ show (length sameStatePairs) ++ " summand pairs for underspecifications...") ]
    resolvePairs sameStatePairs
  where
    resolvePairs :: [(LPESummand, LPESummand)] -> IOC.IOC LPE
    resolvePairs [] = return lpe
    resolvePairs (pair:pairs) = do
        maybeNewLpe <- tryToAddUGuards lpe invariant pair
        case maybeNewLpe of
          Just newLpe -> return newLpe
          Nothing -> resolvePairs pairs
-- doIteration

tryToAddUGuards :: LPE -> TxsDefs.VExpr -> (LPESummand, LPESummand) -> IOC.IOC (Maybe LPE)
tryToAddUGuards lpe invariant (summand1, summand2) = do
    let summands = Set.toList (lpeSummands lpe)
    let sindex1 = Maybe.fromMaybe 0 (List.elemIndex summand1 summands)
    let sindex2 = Maybe.fromMaybe 0 (List.elemIndex summand2 summands)
    successors1 <- getPossibleSuccessors (lpeSummands lpe) invariant summand1
    successors2 <- getDefiniteSuccessors (lpeSummands lpe) invariant summand2
    -- We are looking for an action in successors2 that is not in successors1, so throw out all shared successors:
    let uniqueSuccessors2 = Set.fromList successors2 Set.\\ Set.fromList successors1
    underspecifiedSuccessors2 <- Monad.filterM (isUnderspecifiedSuccessor2 successors1) (Set.toList uniqueSuccessors2)
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("" ++ show (Set.size uniqueSuccessors2) ++ " successors of summand pair (" ++ show sindex1 ++ ", " ++ show sindex2 ++ ") are unique, " ++ show (length underspecifiedSuccessors2) ++ " are underspecified") ]
    if null underspecifiedSuccessors2
    then return Nothing
    else do freshPar <- createFreshBoolVarFromPrefix "__UG"
            
            -- Make the guards of all underspecified successors more strict by adding a conjunct:
            let conjunct = ValExpr.cstrEqual (ValExpr.cstrVar freshPar) (ValExpr.cstrConst (Constant.Cbool False))
            let conjunctedSuccessors2 = map (addConjunctToSuccessor2 conjunct) underspecifiedSuccessors2
            
            -- Replace the underspecified successors:
            let conjunctedSummands = Set.union (lpeSummands lpe Set.\\ Set.fromList underspecifiedSuccessors2) (Set.fromList conjunctedSuccessors2)
            
            -- It is possible that summand1 and summand2 were changed. This is how they look now:
            let conjunctedSummand1 = if summand1 `List.elem` underspecifiedSuccessors2
                                     then addConjunctToSuccessor2 conjunct summand1
                                     else summand1
            let conjunctedSummand2 = if summand2 `List.elem` underspecifiedSuccessors2
                                     then addConjunctToSuccessor2 conjunct summand1
                                     else summand2
            
            -- Except for summand1 and summand2, add the new parameter to the parameter equations:
            let enablingSummands = Set.map (addParamEqToSummand freshPar False) (conjunctedSummands Set.\\ Set.fromList [conjunctedSummand1, conjunctedSummand2])
            
            -- Summand1 and summand2 actually disable the underspecified successors:
            let disablingSummand1 = addParamEqToSummand freshPar True conjunctedSummand1
            let disablingSummand2 = addParamEqToSummand freshPar True conjunctedSummand2
            
            let newLpe = lpe { -- Add summand1 and summand2 back into the mix:
                               lpeSummands = Set.union enablingSummands (Set.fromList [disablingSummand1, disablingSummand2])
                               -- The new parameter must also be added to the model initialization:
                             , lpeInitEqs = addParamEq freshPar False (lpeInitEqs lpe)
                             }
            
            IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Found and disabled " ++ show (length underspecifiedSuccessors2) ++ " underspecified summands") ]
            
            -- Done!
            return (Just newLpe)
  where
    isUnderspecifiedSuccessor2 :: [LPESummand] -> LPESummand -> IOC.IOC Bool
    isUnderspecifiedSuccessor2 successors1 successor2 = do
        -- We are looking for successors1 that can be enabled together with successor2.
        -- This is equivalent to looking for successors1 that 'are non-deterministic with' successor2:
        nonDetSuccs <- Monad.filterM (areSummandsNonDeterministic invariant successor2) successors1
        -- If no such successors1 exists, the action of successor2 is underspecified (at least when successor2 follows summand2):
        return (null nonDetSuccs)
    -- isUnderspecifiedSuccessor2
    
    addConjunctToSuccessor2 :: TxsDefs.VExpr -> LPESummand -> LPESummand
    addConjunctToSuccessor2 conjunct successor2 =
        successor2 { lpeSmdGuard = ValExpr.cstrAnd (Set.fromList [conjunct, lpeSmdGuard successor2]) }
    -- addConjunctToSuccessor2
    
    addParamEqToSummand :: VarId.VarId -> Bool -> LPESummand -> LPESummand
    addParamEqToSummand freshPar freshParValue successor2 =
        successor2 { lpeSmdEqs = addParamEq freshPar freshParValue (lpeSmdEqs successor2) }
    -- addParamEqToSummand
    
    addParamEq :: VarId.VarId -> Bool -> LPEParamEqs -> LPEParamEqs
    addParamEq freshPar freshParValue = Map.insert freshPar (ValExpr.cstrConst (Constant.Cbool freshParValue))
-- tryToAddUGuards

-- The result is an underapproximation!
getSameStateSummandPairs :: TxsDefs.VExpr -> [LPESummand] -> IOC.IOC [(LPESummand, LPESummand)]
getSameStateSummandPairs _ [] = return []
getSameStateSummandPairs invariant summands = Monad.filterM (isSameStateSummandPair invariant) (allPairsNonId summands summands)

-- Check if a summand pair satisfies all of the following conditions:
--  - The guard of the first summand must imply the guard of the second summand (modulo variable renaming);
--  - The channels of the first summand must be equal to the channels of the second summand.
-- This method may return false negatives!
isSameStateSummandPair :: TxsDefs.VExpr -> (LPESummand, LPESummand) -> IOC.IOC Bool
isSameStateSummandPair invariant (summand1, summand2) = do
    let sortedChans1 = List.sortOn (ChanId.unid . fst) (Map.toList (lpeSmdOffers summand1))
    let sortedChans2 = List.sortOn (ChanId.unid . fst) (Map.toList (lpeSmdOffers summand2))
    if map fst sortedChans1 /= map fst sortedChans2
    then return False
    else do let chanVars1 = concatMap snd sortedChans1
            let chanVars2 = concatMap snd sortedChans2
            let useChanVars1 = Map.fromList (zipWith (\cv1 cv2 -> (cv2, ValExpr.cstrVar cv1)) chanVars1 chanVars2)
            guard2' <- doBlindSubst useChanVars1 (lpeSmdGuard summand2)
            let guardEq = ValExpr.cstrEqual (lpeSmdGuard summand1) guard2'
            procInstEqs <- Monad.mapM (getProcInstEq useChanVars1 (lpeSmdEqs summand2)) (Map.toList (lpeSmdEqs summand1))
            taut <- Sat.isTautology (ValExpr.cstrAnd (Set.fromList (guardEq:procInstEqs))) invariant
            if taut
            then return False
            else Sat.isSatisfiable (ValExpr.cstrITE (lpeSmdGuard summand1) guard2' (ValExpr.cstrConst (Constant.Cbool False))) invariant
  where
    getProcInstEq :: Map.Map VarId.VarId TxsDefs.VExpr -> LPEParamEqs -> (VarId.VarId, TxsDefs.VExpr) -> IOC.IOC TxsDefs.VExpr
    getProcInstEq subst eqs2 (p1, v1) = do
        let v2 = eqs2 Map.! p1
        v2' <- doBlindSubst subst v2
        return (ValExpr.cstrEqual v1 v2')
-- isSameStateSummandPair


