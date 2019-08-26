{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LinearizeParallel
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TupleSections       #-}

module LinearizeParallel (
linearize
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ValExpr
import qualified Subst
import qualified ChanId
import qualified VarId
import ActOfferFactory
import ValFactory
import VarFactory
import SortFactory
import BehExprDefs

import BranchLinearityUtils
import UniqueObjects
import ThreadUtils

import MapDebug

type Info = ( [TxsDefs.VExpr] -> TxsDefs.BExpr    -- Function that should be used to recursively instantiate the parent process.
            , [VarId.VarId]                       -- Flag variables that indicate whether a child process has been initialized.
            , [VarId.VarId]                       -- Extra variables that the parent process will have to add to the process declaration.
            , TxsDefs.VExpr                       -- Guard.
            )
-- Info

linearize :: TExprLinearizer
linearize createProcInst g (TxsDefs.view -> Parallel synchronizedChans threads) = do
    -- We require that all thread processes are unique, as well as all variables that they declare!
    threads' <- ensureDistinguishableThreads threads
    Monad.mapM_ ensureFreshVarsInProcInst threads'
    
    -- Extract data from all parallel sub-expressions:
    dataPerThread <- Monad.mapM (getThreadData "initFlag" getBoolSort) threads'
    
    -- List all multi-channels used in the branches;
    -- then partition them into multi-channels that must synchronize and multi-channels that must not.
    let allCidSets = Set.unions (map (Set.map bVizChans . tBranchData) dataPerThread)
    let (syncingCidSets, unsyncedCidSets) = Set.partition (\c -> Set.intersection c synchronizedChans /= Set.empty) allCidSets
    
    -- Compute the multi-channels over which threads can synchronize (they cannot have other channels in common):
    let syncingCidMinSubSets = Set.map (Set.intersection synchronizedChans) syncingCidSets
    
    -- Set variable declarations required by the new branches:
    let initFlags = map tInitVar dataPerThread
    let newVidDecls = concatMap (map fst . tInitEqs) dataPerThread
    
    -- Construct new branches:
    let info = (createProcInst, initFlags, newVidDecls, g)
    syncedBranches <- Set.unions <$> Monad.mapM (synchronizeOneBranchPerThread info dataPerThread syncingCidSets) (Set.toList syncingCidMinSubSets)
    unsyncedBranches <- leaveBranchesUnsynchronized info (map (filterThreadData (`Set.member` unsyncedCidSets)) dataPerThread)
    
    return (TExprLinResult { lrBranches = Set.union syncedBranches unsyncedBranches
                           , lrParams = initFlags ++ newVidDecls
                           , lrPredefInits = Map.fromList (map (, cstrFalse) initFlags)
                           })
linearize _ _ bexpr = error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow bexpr ++ "\")!")

-- Considers all lists consisting of one branch per thread.
-- Synchronizes those branches.
synchronizeOneBranchPerThread :: Info -> [ThreadData] -> Set.Set (Set.Set ChanId.ChanId) -> Set.Set ChanId.ChanId -> IOC.IOC (Set.Set TxsDefs.BExpr)
synchronizeOneBranchPerThread info dataPerThread syncingCidSets syncingCidMinSubSet = do
    let filteredSyncingCidSets = Set.filter (syncingCidMinSubSet `Set.isSubsetOf`) syncingCidSets
    buildList [] (map (filterThreadData (`Set.member` filteredSyncingCidSets)) dataPerThread)
  where
    buildList :: [(BranchData, ThreadData)] -> [ThreadData] -> IOC.IOC (Set.Set TxsDefs.BExpr)
    buildList finalList [] =
        if noOtherSharedChannels (map fst finalList)
        then Set.fromList <$> Monad.mapM (createSyncedBranch info syncingCidMinSubSet finalList) (List.subsequences (map snd finalList))
        else return Set.empty
    buildList listSoFar (td:remaining) =
        Set.unions <$> Monad.mapM (\bd -> buildList (listSoFar ++ [(bd, td)]) remaining) (Set.toList (tBranchData td))
    -- buildList
    
    noOtherSharedChannels :: [BranchData] -> Bool
    noOtherSharedChannels bd = 
        let allNonSyncChans = concatMap (\b -> Set.toList (bVizChans b Set.\\ syncingCidMinSubSet)) bd in
          Set.size (Set.fromList allNonSyncChans) == length allNonSyncChans
    -- noOtherSharedChannels
-- createSyncedBranches

-- Constructs a new branch from a number of synchronizable branches.
createSyncedBranch :: Info -> Set.Set ChanId.ChanId -> [(BranchData, ThreadData)] -> [ThreadData] -> IOC.IOC TxsDefs.BExpr
createSyncedBranch _ _ [] _ = error "There should be at least two elements"
createSyncedBranch _ _ [_] _ = error "There should be at least two elements"
createSyncedBranch info@(_createProcInst, initFlags, newVidDecls, g) synchronizingChans dataPerBranch initializedBranches = do
    -- Create fresh variables for shared communication variables that are related:
    let sharedVars = map (\(bd, _td) -> concatMap (bOfferVarsPerChan bd Map.!) (Set.toList synchronizingChans)) dataPerBranch
    freshVars <- createFreshVars (Set.fromList (head sharedVars))
    let orderedFreshVars = map (getOrError freshVars TxsShow.fshow) (head sharedVars)
    let freshVarPerSharedVar = Map.fromList (concatMap (`zip` orderedFreshVars) sharedVars)
    let applyFreshVars = Subst.subst (Map.map ValExpr.cstrVar freshVarPerSharedVar) Map.empty
    
    -- Rewrite all ActOffers so that they use the shared communication variables.
    -- Also substitute the initialization values of the thread (if the thread has not yet been initialized).
    let getNewActOffer (bd, td) = let applyInitEqs = if td `List.elem` initializedBranches then id else doActOfferSubst (Map.fromList (tInitEqs td)) in
                                    replaceVarsInActOffer freshVarPerSharedVar (applyInitEqs (bActOffer bd))
    let newActOfferPerBExpr = map getNewActOffer dataPerBranch
    
    -- Replace variables in the ParamEqs:
    let getNewParamEqs (bd, td) = let applyInitEqs = if td `List.elem` initializedBranches then id else Subst.subst (Map.fromList (tInitEqs td)) Map.empty in
                                  let vidDeclEqs = Map.map applyFreshVars (Map.map applyInitEqs (bParamEqs bd)) in
                                    Map.insert (tInitVar td) cstrTrue vidDeclEqs
    let newParamEqsPerBExpr = map getNewParamEqs dataPerBranch
    
    -- Construct the new guard:
    let getInitFlagValue flag = if flag `List.elem` map tInitVar initializedBranches
                                then ValExpr.cstrVar flag
                                else ValExpr.cstrNot (ValExpr.cstrVar flag)
    let newGuard = ValExpr.cstrAnd (Set.fromList (g : map getInitFlagValue initFlags))
    
    -- Combine everything:
    let newActOffer = addActOfferConjunct (foldl1 mergeActOffers newActOfferPerBExpr) newGuard
    let newParamEqsWithoutInitFlags = Map.union (Map.unions newParamEqsPerBExpr) (Map.fromSet ValExpr.cstrVar (Set.fromList (initFlags ++ newVidDecls)))
    let newInitFlagValues = Map.fromSet (const cstrTrue) (Set.fromList (map (tInitVar . snd) dataPerBranch))
    let newProcInst = createNewProcInst info newInitFlagValues newParamEqsWithoutInitFlags
    
    let newActionPref = actionPref newActOffer newProcInst
    let hiddenChansPerBExpr = map (\(bd, _td) -> bHidChans bd) dataPerBranch
    return (applyHide (Set.unions hiddenChansPerBExpr) newActionPref)
-- createSyncedBranch

leaveBranchesUnsynchronized :: Info -> [ThreadData] -> IOC.IOC (Set.Set TxsDefs.BExpr)
leaveBranchesUnsynchronized info threadData =
    Set.unions <$> Monad.mapM doSingle threadData
  where
    doSingle :: ThreadData -> IOC.IOC (Set.Set TxsDefs.BExpr)
    doSingle td = do
        let branchData = Set.toList (tBranchData td)
        initBs <- Set.fromList <$> Monad.mapM (\bd -> createUnsyncedBranch info (bd, td) False) branchData
        succBs <- Set.fromList <$> Monad.mapM (\bd -> createUnsyncedBranch info (bd, td) True) branchData
        return (Set.union initBs succBs)
    -- doSingle
-- leaveBranchesUnsynchronized

createUnsyncedBranch :: Info -> (BranchData, ThreadData) -> Bool -> IOC.IOC TxsDefs.BExpr
createUnsyncedBranch info@(_createProcInst, initFlags, newVidDecls, g) (bd, td) isInitialized = do
    let newGuard = ValExpr.cstrAnd (Set.fromList [cstrBoolEq isInitialized (ValExpr.cstrVar (tInitVar td)), g])
    let newActOffer = addActOfferConjunct (bActOffer bd) newGuard
    
    -- Re-use existing ParamEqs if possible (Map.union is left-biased):
    let newParamEqsWithoutInitFlags = Map.union (bParamEqs bd) (Map.fromSet ValExpr.cstrVar (Set.fromList (initFlags ++ newVidDecls)))
    let newInitFlagValues = Map.singleton (tInitVar td) cstrTrue
    let newProcInst = createNewProcInst info newInitFlagValues newParamEqsWithoutInitFlags
    
    let newActionPref = actionPref newActOffer newProcInst
    let applyInitEqs = if isInitialized then id else Subst.subst (Map.fromList (tInitEqs td)) Map.empty
    
    return (applyHide (bHidChans bd) (applyInitEqs newActionPref))
-- createUnsyncedBranch

-- Because LINT wants to reduce duplication so badly...:
createNewProcInst :: Info -> Map.Map VarId.VarId TxsDefs.VExpr -> Map.Map VarId.VarId TxsDefs.VExpr -> TxsDefs.BExpr
createNewProcInst (createProcInst, initFlags, newVidDecls, _g) newInitFlagValues newParamEqsWithoutInitFlags =
    let newParamEqs = Map.union newInitFlagValues newParamEqsWithoutInitFlags in
      createProcInst (map (newParamEqs Map.!) (initFlags ++ newVidDecls))
-- createNewProcInst




