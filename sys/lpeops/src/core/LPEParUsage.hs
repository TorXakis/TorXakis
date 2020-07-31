{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEParUsage
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEParUsage (
LPEParamUsage(..),
showLPEParamUsage,
showLPEParamUsagePerSummand,
getParamUsagePerSummand,
getParamSourcesPerSummand,
getParamDestinationsPerSummand,
getUsedParamsPerSummand,
getChangedParamsPerSummand,
getDirectlyUsedParamsPerSummand
) where

import qualified Control.Monad       as Monad
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified EnvCore             as IOC
import qualified FreeVar
import qualified SolveDefs
import qualified TxsDefs
import           LPETypes
import           LPEPrettyPrint
import qualified Satisfiability as Sat
import           VarId
import           ValExpr
import           VarFactory
import qualified Constant

-- Just for convenience, not used anywhere else:
type ParamsPerSummand = Map.Map LPESummand (Set.Set VarId)

data LPEParamUsage = LPEParamUsage { directlyUsedParams :: Set.Set VarId
                                   , changedParams :: Set.Set VarId
                                   , usedParams :: Set.Set VarId
                                   , rulingParams :: Set.Set VarId
                                   , paramSources :: LPEParamEqs
                                   , paramDestinations :: LPEParamEqs
                                   } deriving (Eq)
-- LPEParamUsage

showLPEParamUsage :: LPEParamUsage -> String
showLPEParamUsage paramUsage =
    "|-> Directly used = {" ++ showVarIds (directlyUsedParams paramUsage) ++ "}\n" ++
    "|-> Changed = {" ++ showVarIds (changedParams paramUsage) ++ "}\n" ++
    "|-> Used = {" ++ showVarIds (usedParams paramUsage) ++ "}\n" ++
    "|-> Ruling = {" ++ showVarIds (rulingParams paramUsage) ++ "}\n" ++
    "|-> Sources = " ++ showSubst (paramSources paramUsage) ++ "\n" ++
    "\\-> Destinations = " ++ showSubst (paramDestinations paramUsage) ++ "\n"
  where
    showVarIds :: Set.Set VarId -> String
    showVarIds vs = List.intercalate ", " (map (Text.unpack . VarId.name) (Set.toList vs))
-- showLPEParamUsage

showLPEParamUsagePerSummand :: Map.Map LPESummand LPEParamUsage -> String
showLPEParamUsagePerSummand paramUsages = concatMap (\(s, m) -> "\nSummand:\n\\-> " ++ showLPESummand Map.empty s ++ "\nParameter usage:\n" ++ m) (Map.toList (Map.map showLPEParamUsage paramUsages))

getParamUsagePerSummand :: LPESummands -> Set.Set VarId -> TxsDefs.VExpr -> IOC.IOC (Map.Map LPESummand LPEParamUsage)
getParamUsagePerSummand summands params invariant = do
    let directlyUsedParamsPerSummand = getDirectlyUsedParamsPerSummand summands params
    changedParamsPerSummand <- getChangedParamsPerSummand summands params invariant
    let usedParamsPerSummand = getUsedParamsPerSummand summands directlyUsedParamsPerSummand changedParamsPerSummand
    paramSourcesPerSummand <- getParamSourcesPerSummand summands params invariant
    paramDestinationsPerSummand <- getParamDestinationsPerSummand summands params invariant
    return (Map.fromSet (\summand -> let summandParamSources = paramSourcesPerSummand Map.! summand in
                                     let summandParamDestinations = paramDestinationsPerSummand Map.! summand in
                                       LPEParamUsage { directlyUsedParams = directlyUsedParamsPerSummand Map.! summand 
                                                     , changedParams      = changedParamsPerSummand Map.! summand
                                                     , usedParams         = usedParamsPerSummand Map.! summand
                                                     , rulingParams       = Map.keysSet (Map.intersection summandParamSources summandParamDestinations)
                                                     , paramSources       = paramSourcesPerSummand Map.! summand
                                                     , paramDestinations  = paramDestinationsPerSummand Map.! summand
                                                     }) summands)
-- getParamUsagePerSummand

getParamSourcesPerSummand :: LPESummands -> Set.Set VarId -> TxsDefs.VExpr -> IOC.IOC (Map.Map LPESummand LPEParamEqs)
getParamSourcesPerSummand summands params invariant = do
    let orderedSummands = Set.toList summands
    dests <- Monad.mapM getParamSources orderedSummands
    return (Map.fromList (zip orderedSummands dests))
  where
    getParamSources :: LPESummand -> IOC.IOC LPEParamEqs
    getParamSources summand = do
        dests <- Monad.mapM (getParamSource summand) (Set.toList params)
        return (Map.unions dests)
    -- getParamSources
    
    getParamSource :: LPESummand -> VarId -> IOC.IOC LPEParamEqs
    getParamSource summand param = do
        guardSolution <- Sat.getUniqueSolution (lpeSmdGuard summand) invariant [] [param]
        case guardSolution of
          SolveDefs.Solved gdSolMap -> return (Map.singleton param (cstrConst (gdSolMap Map.! param)))
          _ -> return Map.empty
    -- getParamSource
-- getParamSourcesPerSummand

getParamDestinationsPerSummand :: LPESummands -> Set.Set VarId -> TxsDefs.VExpr -> IOC.IOC (Map.Map LPESummand LPEParamEqs)
getParamDestinationsPerSummand summands params invariant = do
    let orderedSummands = Set.toList summands
    dests <- Monad.mapM getParamDestinations orderedSummands
    return (Map.fromList (zip orderedSummands dests))
  where
    getParamDestinations :: LPESummand -> IOC.IOC LPEParamEqs
    getParamDestinations summand = do
        dests <- Monad.mapM (getParamDestination summand) (Set.toList params)
        return (Map.unions dests)
    -- getParamDestinations
    
    getParamDestination :: LPESummand -> VarId -> IOC.IOC LPEParamEqs
    getParamDestination summand param = do
        (destVar, destSatExpr) <- constructDestSatExpr summand param
        destSolution <- Sat.getUniqueSolution destSatExpr invariant [] [destVar]
        case destSolution of
          SolveDefs.Solved destSolMap -> return (Map.singleton param (cstrConst (destSolMap Map.! destVar)))
          _ -> return Map.empty
    -- getParamDestination
-- getParamDestinationsPerSummand

-- Finds the parameters that are 'used' by the specified summands.
-- A parameter that is 'used' by a summand is one that:
--  + Occurs in the guard (which means that it is marked as 'directly used'), or
--  + Occurs in the assignment to a variable (in the process instantiation) that is marked as 'changed'.
getUsedParamsPerSummand :: LPESummands -> ParamsPerSummand -> ParamsPerSummand -> ParamsPerSummand
getUsedParamsPerSummand summands directlyUsedParamsPerSummand changedParamsPerSummand =
    Map.fromSet getUsedParams summands
  where
    getUsedParams :: LPESummand -> Set.Set VarId
    getUsedParams summand =
        let changedPars = changedParamsPerSummand Map.! summand in
        let changedParsAssignments = Map.restrictKeys (lpeSmdEqs summand) changedPars in
        let directlyUsedPars = directlyUsedParamsPerSummand Map.! summand in
        let indirectlyUsedPars = Set.unions (map (Set.fromList . FreeVar.freeVars) (Map.elems changedParsAssignments)) in
          Set.intersection (Map.keysSet (lpeSmdEqs summand)) (Set.union indirectlyUsedPars directlyUsedPars)
-- getUsedParamsPerSummand

-- Finds the parameters that are (possibly) changed by a summand, for all specified summands.
getChangedParamsPerSummand :: LPESummands -> Set.Set VarId -> TxsDefs.VExpr -> IOC.IOC ParamsPerSummand
getChangedParamsPerSummand summands params invariant = do
    let orderedSummands = Set.toList summands
    changed <- Monad.mapM getChangedParams orderedSummands
    return (Map.fromList (zip orderedSummands changed))
  where
    getChangedParams :: LPESummand -> IOC.IOC (Set.Set VarId)
    getChangedParams summand = do
        unchangedParams <- Monad.filterM (isParamUnchanged summand) (Set.toList params)
        return (params Set.\\ Set.fromList unchangedParams)
    -- getChangedParams
    
    isParamUnchanged :: LPESummand -> VarId -> IOC.IOC Bool
    isParamUnchanged summand param = do
        (destVar, destSatExpr) <- constructDestSatExpr summand param
        Sat.isTautology (cstrITE destSatExpr (cstrEqual (cstrVar destVar) (cstrVar param)) (cstrConst (Constant.Cbool True))) invariant
-- getChangedParamsPerSummand

-- Finds the parameters that are 'directly used' by the specified summands.
-- A parameter that is 'directly used' by a summand is one that occurs in the guard.
-- (Normally, the parameters used in action arguments are also considered to be 'directly used', but
-- in TorXakis all action arguments are of the form '? x').
getDirectlyUsedParamsPerSummand :: LPESummands -> Set.Set VarId -> ParamsPerSummand
getDirectlyUsedParamsPerSummand summands params = Map.fromSet getDirectlyUsedParams summands
  where
    getDirectlyUsedParams :: LPESummand -> Set.Set VarId
    getDirectlyUsedParams summand = Set.intersection (Set.fromList (FreeVar.freeVars (lpeSmdGuard summand))) params
-- getDirectlyUsedParamsPerSummand

-- The function returns a pair:
--   The first element is the variable for which must be solved in order
--   to determine the value of a parameter after a summand.
--   The second element is the expression with the variable that must be solved.
-- There are two use cases:
--   1. Looking for the value of a parameter after a summand by solving the expression for the variable.
--   2. Determining whether the value of a parameter is unaffected by a summand
--      by checking if 'expression && p2 == p1' is a tautology. Here,
--      p2 is the first element in the pair returned by this function, and
--      p1 is the variable provided as the second parameter to this function.
constructDestSatExpr :: LPESummand -> VarId -> IOC.IOC (VarId, TxsDefs.VExpr)
constructDestSatExpr summand param = do
    paramClone <- createFreshVarFromVar param
    let eq = cstrAnd (Set.fromList [lpeSmdGuard summand, cstrEqual (cstrVar paramClone) (lpeSmdEqs summand Map.! param)])
    -- IOC.putMsgs [ EnvData.TXS_CORE_ANY ("destSatExpr for " ++ (Text.unpack (VarId.name param)) ++ "/" ++ (Text.unpack (VarId.name paramClone)) ++ " is " ++ (showValExpr eq)) ]
    return (paramClone, eq)
-- constructDestSatExpr

