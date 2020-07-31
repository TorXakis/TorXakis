{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEDataReset
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEDataReset (
dataReset
) where

import qualified Control.Monad as Monad
-- import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified FreeVar
import qualified TxsDefs
import qualified EnvCore as IOC
import qualified EnvData
import qualified VarId
import LPETypes
import LPEParUsage
import LPEPrettyPrint
import UntilFixedPoint

mapGet :: (Show a, Ord a) => Map.Map a b -> a -> b
mapGet m k =
    --trace ("mapGet(" ++ (show k) ++ ")") (
      if Map.member k m
      then m Map.! k
      else error ("Could not find " ++ show k ++ " in map!")
    --)
-- mapGet

dataReset :: LPEOperation
dataReset lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<datareset>>" ]
    let params = lpeParams lpe
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "Collecting information on parameter usage..." ]
    paramUsagePerSummand <- getParamUsagePerSummand (lpeSummands lpe) params invariant
    -- IOC.putMsgs [ EnvData.TXS_CORE_ANY (showLPEParamUsagePerSummand paramUsagePerSummand) ]
    let controlFlowParams = getControlFlowParams paramUsagePerSummand params
    let dataParams = params Set.\\ controlFlowParams
    let belongsToRelation = getBelongsToRelation paramUsagePerSummand controlFlowParams dataParams
    --Monad.mapM_ (\(d, cfps) -> IOC.putMsgs [ EnvData.TXS_CORE_ANY ((Text.unpack (VarId.VarId.name d)) ++ " belongs to " ++ (List.intercalate ", " (map (Text.unpack . VarId.VarId.name) cfps))) ]) (Map.toList belongsToRelation)
    let controlFlowGraphs = getControlFlowGraphs paramUsagePerSummand controlFlowParams
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "Building relevance relation..." ]
    let initialRelevanceRelation = concat [
                                     concat [
                                       [ (dk, dj, s) | (dj', s) <- Map.toList (paramSources paramUsage), dj' == dj ]
                                     | dj <- djs, paramUsage <- Map.elems paramUsagePerSummand, dk `elem` directlyUsedParams paramUsage ]
                                   | (dk, djs) <- Map.toList belongsToRelation ]
    let relevanceRelation = untilFixedPoint (updateRelevanceRelation paramUsagePerSummand controlFlowGraphs belongsToRelation) (Set.fromList initialRelevanceRelation)
    -- IOC.putMsgs [ EnvData.TXS_CORE_ANY (showRelevanceRelation "Full relevance relation:" relevanceRelation) ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "Constructing new LPE..." ]
    let newSummands = map (resetParamsInSummand (lpeInitEqs lpe) paramUsagePerSummand belongsToRelation relevanceRelation) (Set.toList (lpeSummands lpe))
    Monad.mapM_ (\m -> IOC.putMsgs [ EnvData.TXS_CORE_ANY m ]) (concatMap snd newSummands)
    return (Right (lpe { lpeSummands = Set.fromList (map fst newSummands) }))
-- dataReset

-- showRelevanceRelation :: String -> Set.Set (VarId.VarId, VarId.VarId, TxsDefs.VExpr) -> String
-- showRelevanceRelation caption relation =
    -- caption ++ "\n" ++ concatMap (\(d, c, v) -> "    " ++ ((Text.unpack (VarId.VarId.name d)) ++ " is relevant when " ++ (Text.unpack (VarId.VarId.name c)) ++ " equals " ++ (showValExpr v) ++ "\n")) (Set.toList relation)
-- showRelevanceRelation

resetParamsInSummand :: Map.Map VarId.VarId TxsDefs.VExpr             -- initParamEqs
                     -> Map.Map LPESummand LPEParamUsage        -- paramUsagePerSummand
                     -> Map.Map VarId.VarId [VarId.VarId]                   -- belongsToRelation
                     -> Set.Set (VarId.VarId, VarId.VarId, TxsDefs.VExpr)   -- relevanceRelation
                     -> LPESummand                              -- summand
                     -> (LPESummand, [String])                  -- result
resetParamsInSummand initParamEqs paramUsagePerSummand belongsToRelation relevanceRelation summand =
    let paramEqs = lpeSmdEqs summand in
    let paramUsage = paramUsagePerSummand Map.! summand in
    let newParamEqs = map (resetParam paramUsage) (Map.toList paramEqs) in
      --(LPESummand channelVars channelOffers guard (LPEProcInst (Map.fromList (map fst newParamEqs))), ["{", showLPESummand summand] ++ concat (map snd newParamEqs) ++ concat (map getParamChange (map fst newParamEqs)) ++ ["}"])
      (summand { lpeSmdEqs = Map.fromList (map fst newParamEqs) }, concatMap (getParamChange . fst) newParamEqs)
  where
    resetParam :: LPEParamUsage -> (VarId.VarId, TxsDefs.VExpr) -> ((VarId.VarId, TxsDefs.VExpr), [String])
    resetParam paramUsage (p, v) =
        let requiredElements = Set.fromList (
                                 concat [
                                   [ (dk, dj, mapGet (paramDestinations paramUsage) dj) | dj <- djs, dj `elem` rulingParams paramUsage ]
                                 | (dk, djs) <- Map.toList belongsToRelation, dk == p ]
                               ) in
          if requiredElements `Set.isSubsetOf` relevanceRelation
          then ((p, v), []) -- showRelevanceRelation ("Elements that show relevance of " ++ (Text.unpack (VarId.VarId.name p)) ++ ":") requiredElements])
          else ((p, mapGet initParamEqs p), []) -- showRelevanceRelation ("Elements that show irrelevance of " ++ (Text.unpack (VarId.VarId.name p)) ++ ":") requiredElements])
    -- resetParam
    
    getParamChange :: (VarId.VarId, TxsDefs.VExpr) -> [String]
    getParamChange (p, v) =
        let paramEqs = lpeSmdEqs summand in
          ["Setting " ++ Text.unpack (VarId.name p) ++ " to " ++ showValExpr v ++ " instead of " ++ showValExpr (mapGet paramEqs p) | mapGet paramEqs p /= v]
    -- getParamChange
-- resetParamsInSummand

updateRelevanceRelation :: Map.Map LPESummand LPEParamUsage                             -- paramUsagePerSummand
                        -> Map.Map VarId.VarId [(TxsDefs.VExpr, LPESummand, TxsDefs.VExpr)]   -- controlFlowGraphs
                        -> Map.Map VarId.VarId [VarId.VarId]                                        -- belongsToRelation
                        -> Set.Set (VarId.VarId, VarId.VarId, TxsDefs.VExpr)                        -- relevanceRelationSoFar
                        -> Set.Set (VarId.VarId, VarId.VarId, TxsDefs.VExpr)                        -- result
updateRelevanceRelation paramUsagePerSummand controlFlowGraphs belongsToRelation relevanceRelationSoFar =
    let update1 = concat [
                    concat [
                      concat [
                        [ (dk, dj, s) | (s, i, t') <- controlFlowGraphs Map.! dj, t' == t, dk `elem` extractParamEqVars i dl ]
                        | (dl, dj', t) <- Set.toList relevanceRelationSoFar, dj' == dj ]
                      | dj <- djs ]
                    | (dk, djs) <- Map.toList belongsToRelation ] in
    let relevanceRelationAfterUpdate1 = Set.union relevanceRelationSoFar (Set.fromList update1) in
    let update2 = concat [
                    concat [
                      concat [
                        concat [
                          [ (dk, dj, s) | (dj', s) <- Map.toList (paramSources (mapGet paramUsagePerSummand i)), dj' == dj ]
                        | (_r, i, t') <- controlFlowGraphs Map.! dp, t' == t, dk `elem` extractParamEqVars i dl ]
                      | (dl', dp, t) <- Set.toList relevanceRelationAfterUpdate1, dl' == dl ]
                    | dj <- dkjs, dj `notElem` dljs ]
                  | (dk, dkjs) <- Map.toList belongsToRelation, (dl, dljs) <- Map.toList belongsToRelation ] in
    let relevanceRelationAfterUpdate2 = Set.union relevanceRelationAfterUpdate1 (Set.fromList update2) in
      relevanceRelationAfterUpdate2
-- updateRelevanceRelation

extractParamEqVars :: LPESummand -> VarId.VarId -> [VarId.VarId]
extractParamEqVars summand varId =
    let paramEqs = lpeSmdEqs summand in
    let assignmentVars = Set.fromList (FreeVar.freeVars (mapGet paramEqs varId)) in
      Set.toList (Set.intersection (Map.keysSet paramEqs) assignmentVars)
-- extractParamEqVars

-- Determines which of the specified data parameters belong to which (set of) specified control flow parameters.
-- A data parameter belongs to a control flow parameter if the data parameter is only changed or used in summands that are ruled by the control flow parameter.
getBelongsToRelation :: Map.Map LPESummand LPEParamUsage -> Set.Set VarId.VarId -> Set.Set VarId.VarId -> Map.Map VarId.VarId [VarId.VarId]
getBelongsToRelation paramUsagePerSummand controlFlowParams dataParams =
    let whereDataParamsAreChangedOrUsed = Map.fromSet getWhereDataParamIsChangedOrUsed dataParams in
      Map.map (Set.toList . getCfpsToWhichDataParamsBelong) whereDataParamsAreChangedOrUsed
  where
    getWhereDataParamIsChangedOrUsed :: VarId.VarId -> Map.Map LPESummand LPEParamUsage
    getWhereDataParamIsChangedOrUsed param = Map.filter (\paramUsage -> (param `elem` changedParams paramUsage) || (param `elem` usedParams paramUsage)) paramUsagePerSummand
    
    getCfpsToWhichDataParamsBelong :: Map.Map LPESummand LPEParamUsage -> Set.Set VarId.VarId
    getCfpsToWhichDataParamsBelong whereDataParamIsChangedOrUsed =
        let rulingParamsWhereDataParamIsChangedOrUsed = map rulingParams (Map.elems whereDataParamIsChangedOrUsed) in
          foldl Set.intersection controlFlowParams rulingParamsWhereDataParamIsChangedOrUsed
    -- getCfpsToWhichDataParamsBelong
-- getBelongsToRelation

getControlFlowGraphs :: Map.Map LPESummand LPEParamUsage -> Set.Set VarId.VarId -> Map.Map VarId.VarId [(TxsDefs.VExpr, LPESummand, TxsDefs.VExpr)]
getControlFlowGraphs paramUsagePerSummand =
    Map.fromSet (Set.toList . getControlFlowGraph)
  where
    getControlFlowGraph :: VarId.VarId -> Set.Set (TxsDefs.VExpr, LPESummand, TxsDefs.VExpr)
    getControlFlowGraph controlFlowParam = Set.unions (map (getControlFlowGraphEdges controlFlowParam) (Map.toList paramUsagePerSummand))
    
    getControlFlowGraphEdges :: VarId.VarId -> (LPESummand, LPEParamUsage) -> Set.Set (TxsDefs.VExpr, LPESummand, TxsDefs.VExpr)
    getControlFlowGraphEdges controlFlowParam (summand, paramUsage) =
        if controlFlowParam `elem` rulingParams paramUsage
        then let paramSource = paramSources paramUsage Map.! controlFlowParam in
             let paramDestination = paramDestinations paramUsage Map.! controlFlowParam in
               Set.singleton (paramSource, summand, paramDestination)
        else Set.empty
-- getControlFlowGraphs

-- Determines which of the specified parameters are 'control flow parameters'; that is,
-- parameters that may only be changed by a summand if they 'rule' that summand (see getRulingParamsPerSummand).
-- This function requires information about which parameters are ruling the summands of the LPE; typically,
-- getRulingParamsPerSummand is used to obtain this information.
getControlFlowParams :: Map.Map LPESummand LPEParamUsage -> Set.Set VarId.VarId -> Set.Set VarId.VarId
getControlFlowParams paramUsagePerSummand =
    Set.filter isControlFlowParam
  where
    isControlFlowParam :: VarId.VarId -> Bool
    isControlFlowParam param = Map.filter (isUnchangedOrRulingParam param) paramUsagePerSummand == paramUsagePerSummand
    
    isUnchangedOrRulingParam :: VarId.VarId -> LPEParamUsage -> Bool
    isUnchangedOrRulingParam param paramUsage =
        let unchanged = param `notElem` changedParams paramUsage in
        let ruling = param `elem` rulingParams paramUsage in
          unchanged || ruling
-- getControlFlowParams

