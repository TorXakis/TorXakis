{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEParReset
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEParReset (
parReset
) where

import qualified Control.Monad       as Monad
import qualified Data.Map            as Map
import qualified Data.List           as List
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified EnvCore             as IOC
import qualified FreeVar
import qualified EnvData
import qualified TxsDefs
import qualified Satisfiability as Sat
import           LPETypes
import           VarId
import           ValExpr
import           LPESuccessors
import           LPEPrettyPrint
import           BlindSubst

--import Debug.Trace

mapGet :: (Show a, Ord a) => Map.Map a b -> a -> b
mapGet m k =
    --trace ("mapGet(" ++ (show k) ++ ")") (
      if Map.member k m
      then m Map.! k
      else error ("Could not find " ++ show k ++ " in map!")
    --)
-- mapGet

mapGetS :: (Show a, Ord a) => LPE -> LPESummand -> Map.Map a b -> a -> b
mapGetS i s m k =
    --trace ("mapGetS(" ++ (show k) ++ ")") (
      if Map.member k m
      then m Map.! k
      else error ("Could not find " ++ show k ++ " in map!\nSummand: " ++ showLPESummand Map.empty s ++ "\nLPE: " ++ showLPE i)
    --)
-- mapGetS

-- LPE rewrite method.
-- Eliminates parameters that do not contribute to the behavior of a process from an LPE.
-- State spaces before and after are strongly bisimilar.
parReset :: LPEOperation
parReset lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<parreset>>" ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "Identifying successors..." ]
    possibleSuccessors <- Monad.mapM (getPossibleSuccessors (lpeSummands lpe) invariant) (Set.toList (lpeSummands lpe))
    let successorsPerSummand = zipWith (\s i -> (s, i, Map.keys (lpeInitEqs lpe))) (Set.toList (lpeSummands lpe)) possibleSuccessors
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "Analyzing control flow..." ]
    newLpe <- parResetLoop lpe invariant successorsPerSummand
    return (Right newLpe)
-- parReset

-- Updates the information collected about summands, in particular their lists of used variables,
-- until the information no longer changes.
-- With the final information, assign ANY values to variables that are unused:
parResetLoop :: LPE -> TxsDefs.VExpr -> [(LPESummand, [LPESummand], [VarId])] -> IOC.IOC LPE
parResetLoop lpe invariant successorsPerSummand = do
    let newSuccessorsPerSummand = parResetUpdate lpe successorsPerSummand
    if newSuccessorsPerSummand == successorsPerSummand
    then do newSummands <- Monad.mapM (resetParamsInSummand lpe invariant successorsPerSummand) (Set.toList (lpeSummands lpe))
            return (lpe { lpeSummands = Set.fromList newSummands })
    else parResetLoop lpe invariant newSuccessorsPerSummand
-- parResetLoop

resetParamsInSummand :: LPE -> TxsDefs.VExpr -> [(LPESummand, [LPESummand], [VarId])] -> LPESummand -> IOC.IOC LPESummand
resetParamsInSummand lpe invariant successorsPerSummand summand =
    case [ (sucs, uvars) | (smd, sucs, uvars) <- successorsPerSummand, smd == summand ] of
      [(sucs, uvars)] -> if length uvars == Map.size (lpeInitEqs lpe)
                         then return summand -- (All variables are used, apparently, so do not touch the summand.)
                         else do let nonSuccessors = Set.toList (lpeSummands lpe Set.\\ Set.fromList sucs)
                                 let newSmdEqs = Map.union (Map.filterWithKey (\p _ -> p `elem` uvars) (lpeSmdEqs summand)) (Map.filterWithKey (\p _ -> p `notElem` uvars) (lpeInitEqs lpe))
                                 constraints <- Monad.mapM (summandToConstraint newSmdEqs) nonSuccessors
                                 notSat <- Sat.areNotSatisfiable constraints invariant
                                 if notSat
                                 then do printNewSmdEqs newSmdEqs
                                         return (summand { lpeSmdEqs = newSmdEqs })
                                 else return summand
      _ -> return summand
  where
    printNewSmdEqs :: LPEParamEqs -> IOC.IOC ()
    printNewSmdEqs newSmdEqs = do
        let changedParamEqs = Map.filterWithKey (\p v -> v /= mapGet (lpeSmdEqs summand) p) newSmdEqs
        let Just summandNumber = List.elemIndex summand (Set.toList (lpeSummands lpe))
        Monad.mapM_ (\(p, v) -> IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Setting " ++ Text.unpack (VarId.name p) ++ " to " ++ showValExpr v ++ " instead of " ++ showValExpr (mapGet (lpeSmdEqs summand) p) ++ " in " ++ numberToString (summandNumber + 1) ++ " summand") ]) (Map.toList changedParamEqs)
    -- printNewSmdEqs
    
    summandToConstraint :: LPEParamEqs -> LPESummand -> IOC.IOC TxsDefs.VExpr
    summandToConstraint newSmdEqs s = do
        g <- doBlindSubst newSmdEqs (lpeSmdGuard s)
        return (cstrAnd (Set.fromList [lpeSmdGuard s, g]))
    -- summandToConstraint
    
    numberToString :: Int -> String
    numberToString number =
        show number ++
        (case number `mod` 10 of
          1 -> "st"
          2 -> "nd"
          3 -> "rd"
          _ -> "th")
    -- numberToString
-- resetParamsInSummand

-- Updates the information collected about summands, in particular their lists of used variables:
parResetUpdate :: LPE -> [(LPESummand, [LPESummand], [VarId])] -> [(LPESummand, [LPESummand], [VarId])]
parResetUpdate lpe successorsPerSummand = map updateSummand successorsPerSummand
  where
    -- Initially, all variables are added to the list of used variables of a summand.
    -- They are removed only if:
    --  * They occur in the condition of the summand while not being defined as a communication variable, e.g. "CHANNEL ? x"; or
    --  * They are used in the assignment to a variable that IS used by a potential successor summand.
    updateSummand :: (LPESummand, [LPESummand], [VarId]) -> (LPESummand, [LPESummand], [VarId])
    updateSummand (summand, successors, _usedVars) =
        let relevantToSuccessorVars = Set.unions (map getRelevantToSuccessorVars successors) in
          (summand, successors, Set.toList relevantToSuccessorVars)
    
    getRelevantToSuccessorVars :: LPESummand -> Set.Set VarId
    getRelevantToSuccessorVars successor =
        let usedVars = concat [uvars | (s, _g, uvars) <- successorsPerSummand, s == successor] in
        
        -- Parameters in the guard are relevant to the successor, because they enable/disable the channel+instantiation:
        let guardVars = Set.fromList (FreeVar.freeVars (lpeSmdGuard successor)) in
        
        -- Parameters used in assignments to used variables are relevant (because the variables are used):
        let assignmentVars = Set.fromList (concat [FreeVar.freeVars (mapGetS lpe successor (lpeSmdEqs successor) u) | u <- usedVars]) in
        
        -- Combine them all, but ignore communication variables:
        let allVars = Set.union guardVars assignmentVars in
          allVars Set.\\ lpeSmdVarSet successor
-- parResetUpdate

