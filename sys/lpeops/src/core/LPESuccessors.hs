{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPESuccessors
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPESuccessors (
isPossibleSuccessor,
getPossibleSuccessors,
LPEPossibleSuccessorMap,
getPossibleSuccessorMap,
showPossibleSuccessorMap,
showSummandIndexList,
printPossibleSuccessorMap,
isDefiniteSuccessor,
getDefiniteSuccessors,
isPossiblePredecessor,
getPossiblePredecessors,
couldHavePredecessor
) where

import qualified Control.Monad       as Monad
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified EnvCore             as IOC
import qualified Satisfiability      as Sat
import qualified TxsDefs
import qualified ValExpr
import           LPETypes
import           LPEBlindSubst
import           VarFactory

isPossibleSummandSeq :: TxsDefs.VExpr -> [LPESummand] -> IOC.IOC Bool
isPossibleSummandSeq invariant [] = Sat.couldBeSatisfiable Sat.defaultInvariant invariant
isPossibleSummandSeq invariant (smd:smds) = do
    cs <- getConditions [] (smd:smds) (selfLoopParamEqs (lpeSmdParams smd))
    Sat.couldBeSatisfiable (ValExpr.cstrAnd (Set.fromList cs)) invariant
  where
    getConditions :: [TxsDefs.VExpr] -> [LPESummand] -> LPEParamEqs -> IOC.IOC [TxsDefs.VExpr]
    getConditions soFar [] _ = return soFar
    getConditions soFar (x:xs) prevSmdEqs = do
        freshVarPerCommVar <- createFreshVars (lpeSmdVarSet x)
        let freshVarSubst = Map.union prevSmdEqs (Map.map ValExpr.cstrVar freshVarPerCommVar)
        g1 <- doBlindSubst freshVarSubst (lpeSmdGuard x)
        --g2 <- doBlindSubst prevSmdEqs g1
        newSmdEqs <- doBlindParamEqsSubst freshVarSubst (lpeSmdEqs x)
        getConditions (g1:soFar) xs newSmdEqs
-- isPossibleSummandSeq

isPossibleSuccessor :: LPESummand -> TxsDefs.VExpr -> LPESummand -> IOC.IOC Bool
isPossibleSuccessor summand invariant candidate = isPossibleSummandSeq invariant [summand, candidate]
    -- freshVarPerCommVar <- createFreshVars (lpeSmdVarSet candidate)
    -- g1 <- doBlindSubst (Map.map ValExpr.cstrVar freshVarPerCommVar) (lpeSmdGuard candidate)
    -- g2 <- doBlindSubst (lpeSmdEqs summand) g1
    -- let g3 = ValExpr.cstrAnd (Set.fromList [lpeSmdGuard summand, g2])
    -- Sat.couldBeSatisfiable g3 invariant
-- -- isPossibleSuccessor

-- Selects all potential successors summands of a given summand from a list with all summands.
-- (In actuality, an overapproximation of all potential successors is selected, namely those
-- whose guard can be satisfied after the guard of the current summand has been satisfied and
-- after the substitutions of the process recursion have taken place.)
getPossibleSuccessors :: LPESummands -> TxsDefs.VExpr -> LPESummand -> IOC.IOC [LPESummand]
getPossibleSuccessors candidates invariant summand =
    Monad.filterM (isPossibleSuccessor summand invariant) (Set.toList candidates)
-- getPossibleSuccessors

type LPEPossibleSuccessorMap = Map.Map [LPESummand] (Set.Set LPESummand)

getPossibleSuccessorMap :: LPE -> TxsDefs.VExpr -> Int -> IOC.IOC LPEPossibleSuccessorMap
getPossibleSuccessorMap lpe invariant maxKeyLength =
    iter initialMap initialMap 0
  where
    -- Without a known history, we assume all summands are possible:
    initialMap :: LPEPossibleSuccessorMap
    initialMap = Map.singleton [] (lpeSummands lpe)
    
    -- In this function, keys are added with a length of (keyLength + 1):
    iter :: LPEPossibleSuccessorMap -> LPEPossibleSuccessorMap -> Int -> IOC.IOC LPEPossibleSuccessorMap
    iter soFar fringe keyLength =
        if keyLength >= maxKeyLength
        then return soFar
        else do IOC.putInfo [ "Computing possible successor map (depth=" ++ show keyLength ++ "/" ++ show maxKeyLength ++ ", #successors=" ++ show (sum (map Set.size (Map.elems fringe))) ++ ")..." ]
                let newSmdSeqs = concat [[ smdSeq ++ [s] | s <- Set.toList succs ] | (smdSeq, succs) <- Map.toList fringe ]
                let prelimSuccsPerSeq = [ (newSmdSeq, Map.findWithDefault Set.empty (tail newSmdSeq) soFar) | newSmdSeq <- newSmdSeqs ]
                refinedSuccsPerSeq <- Map.fromList <$> Monad.mapM refineSuccsPerSeq prelimSuccsPerSeq
                iter (Map.union soFar refinedSuccsPerSeq) refinedSuccsPerSeq (keyLength + 1)
    -- iter
    
    refineSuccsPerSeq :: ([LPESummand], Set.Set LPESummand) -> IOC.IOC ([LPESummand], Set.Set LPESummand)
    refineSuccsPerSeq (smdSeq, succs) = do
        newSuccs <- Monad.filterM (\s -> isPossibleSummandSeq invariant (smdSeq ++ [s])) (Set.toList succs)
        return (smdSeq, Set.fromList newSuccs)
    -- refineSuccsPerSeq
-- getPossibleSuccessorMap

showPossibleSuccessorMap :: LPE -> LPEPossibleSuccessorMap -> [String]
showPossibleSuccessorMap lpe possibleSuccessorMap = map showEntry (Map.toList possibleSuccessorMap)
  where
    showEntry :: ([LPESummand], Set.Set LPESummand) -> String
    showEntry (smdSeq, succs) = "[" ++ showSummandIndexList lpe smdSeq ++ "] -> {" ++ showSummandIndexList lpe (Set.toList succs) ++ "}"
-- showPossibleSuccessorMap

showSummandIndexList :: LPE -> [LPESummand] -> String
showSummandIndexList lpe summands = List.intercalate ", " (map showSummand summands)
  where
    showSummand :: LPESummand -> String
    showSummand smd =
        case List.elemIndex smd (lpeSmdList lpe) of
          Just i -> show i
          Nothing -> "?"
    -- showSummand
-- showSummandIndexList

printPossibleSuccessorMap :: LPE -> LPEPossibleSuccessorMap -> IOC.IOC ()
printPossibleSuccessorMap lpe possibleSuccessorMap =
    IOC.putInfo (showPossibleSuccessorMap lpe possibleSuccessorMap)
-- printPossibleSuccessorMap

-- getPossibleSuccessorMap :: LPE -> TxsDefs.VExpr -> IOC.IOC LPEPossibleSuccessorMap
-- getPossibleSuccessorMap lpe invariant =
    -- Map.fromList <$> Monad.mapM getKeyValuePair (Set.toList (lpeSummands lpe))
  -- where
    -- getKeyValuePair :: LPESummand -> IOC.IOC (LPESummand, Set.Set LPESummand)
    -- getKeyValuePair summand = do
      -- value <- Set.fromList <$> getPossibleSuccessors (lpeSummands lpe) invariant summand
      -- return (summand, value)
-- -- getPossibleSuccessors

isDefiniteSuccessor :: LPESummand -> TxsDefs.VExpr -> LPESummand -> IOC.IOC Bool
isDefiniteSuccessor summand invariant candidate = do
    g3 <- doBlindSubst (lpeSmdEqs summand) (lpeSmdGuard candidate)
    Sat.isTautology g3 invariant
-- isDefiniteSuccessor

-- Selects all summands from a given list that are definitely successors of a given summand.
-- The result is an underapproximation!
getDefiniteSuccessors :: LPESummands -> TxsDefs.VExpr -> LPESummand -> IOC.IOC [LPESummand]
getDefiniteSuccessors summands invariant summand =
    Monad.filterM (isDefiniteSuccessor summand invariant) (Set.toList summands)
-- getDefiniteSuccessors

isPossiblePredecessor :: LPESummand -> TxsDefs.VExpr -> LPESummand -> IOC.IOC Bool
isPossiblePredecessor summand invariant candidate = isPossibleSuccessor candidate invariant summand

getPossiblePredecessors :: LPESummands -> TxsDefs.VExpr -> LPESummand -> IOC.IOC [LPESummand]
getPossiblePredecessors summands invariant summand =
    Monad.filterM (isPossiblePredecessor summand invariant) (Set.toList summands)
-- getPossiblePredecessors

couldHavePredecessor :: LPESummands -> TxsDefs.VExpr -> LPESummand -> IOC.IOC Bool
couldHavePredecessor summands invariant summand = do
    preds <- getPossiblePredecessors summands invariant summand
    return (not (List.null preds))
-- couldHavePredecessor

