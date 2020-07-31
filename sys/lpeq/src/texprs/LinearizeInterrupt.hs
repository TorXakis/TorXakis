{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LinearizeInterrupt
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module LinearizeInterrupt (
linearize
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ValExpr
import qualified Subst
import qualified VarId
import ActOfferFactory
import ValFactory
import SortFactory
import BehExprDefs

import BranchLinearityUtils
import ThreadUtils
import UniqueObjects

type Info = ( [TxsDefs.VExpr] -> TxsDefs.BExpr    -- Function that should be used to recursively instantiate the parent process.
            , VarId.VarId                         -- Variable that indicates the state of thread1 (idle, inited, paused+idle, paused+inited).
            , VarId.VarId                         -- Variable that indicates the state of thread2 (idle, inited, cancelled).
                                                  -- This means that the thread cannot (or can no longer) cause new behaviour!
            , [VarId.VarId]                       -- Extra variables that the parent process will have to add to the process declaration.
            , TxsDefs.VExpr                       -- Guard.
            )
-- Info

getExitFlagIdle :: Integer
getExitFlagIdle = 0

getExitFlagInited :: Integer
getExitFlagInited = 1

getExitFlagCancelled :: Integer
getExitFlagCancelled = 2

getExitFlagPausedAndIdle :: Integer
getExitFlagPausedAndIdle = 3

getExitFlagPausedAndInited :: Integer
getExitFlagPausedAndInited = 4

linearize :: TExprLinearizer
linearize createProcInst g (TxsDefs.view -> Interrupt thread1 thread2) = do
    -- We require that the two thread processes are unique, as well as all variables that they declare!
    [thread1', thread2'] <- ensureDistinguishableThreads [thread1, thread2]
    Monad.mapM_ ensureFreshVarsInProcInst [thread1', thread2']
    
    threadData1 <- getThreadData "initFlag" getIntSort thread1'
    let (exitThreadData1, nonExitThreadData1) = partitionThreadData (Set.member chanIdExit) threadData1
    let exitBranchData1 = Set.toList (tBranchData exitThreadData1)
    let nonExitBranchData1 = Set.toList (tBranchData nonExitThreadData1)
    
    threadData2 <- getThreadData "initFlag" getIntSort thread2'
    let (exitThreadData2, nonExitThreadData2) = partitionThreadData (Set.member chanIdExit) threadData2
    let exitBranchData2 = Set.toList (tBranchData exitThreadData2)
    let nonExitBranchData2 = Set.toList (tBranchData nonExitThreadData2)
    
    let initFlag1 = tInitVar threadData1
    let initFlag2 = tInitVar threadData2
    let newVidDecls = map fst (tInitEqs threadData1) ++ map fst (tInitEqs threadData2)
    let info = (createProcInst, initFlag1, initFlag2, newVidDecls, g)
    
    -- Uninitialized thread1 can take actions while thread2 is idle (and not cancelled, which implies that thread1 is already initialized).
    -- Initialized thread1 can take actions while thread2 is idle or cancelled.
    lhsUninitedNonExitBranches <- Monad.mapM (createLhsNonExitBranch info getExitFlagIdle getExitFlagIdle threadData1) nonExitBranchData1
    lhsInitedNonExitBranches1 <- Monad.mapM (createLhsNonExitBranch info getExitFlagInited getExitFlagIdle threadData1) nonExitBranchData1
    lhsInitedNonExitBranches2 <- Monad.mapM (createLhsNonExitBranch info getExitFlagInited getExitFlagCancelled threadData1) nonExitBranchData1
    
    -- thread1 can cancel thread2 if thread2 is idle:
    lhsUninitedExitBranches <- Monad.mapM (createLhsExitBranch info getExitFlagIdle getExitFlagIdle threadData1) exitBranchData1
    lhsInitedExitBranches <- Monad.mapM (createLhsExitBranch info getExitFlagInited getExitFlagIdle threadData1) exitBranchData1
    
    -- thread1 can do EXIT if thread2 has already been cancelled (which implies that thread1 is already initialized):
    lhsInitedSemiExitBranches <- Monad.mapM (createLhsNonExitBranch info getExitFlagInited getExitFlagCancelled threadData1) nonExitBranchData1
    
    -- thread2 can become initialized, disabling thread1:
    rhsUninitedNonExitBranches <- Monad.mapM (createRhsNonExitBranch info getExitFlagIdle getExitFlagPausedAndIdle getExitFlagIdle threadData2) nonExitBranchData2
    rhsInitedNonExitBranches <- Monad.mapM (createRhsNonExitBranch info getExitFlagInited getExitFlagPausedAndInited getExitFlagInited threadData2) nonExitBranchData2
    
    -- thread2 can do EXIT, re-enabling thread1:
    rhsUninitedExitBranches1 <- Monad.mapM (createRhsExitBranch info getExitFlagPausedAndIdle getExitFlagIdle getExitFlagIdle threadData2) exitBranchData2
    rhsUninitedExitBranches2 <- Monad.mapM (createRhsExitBranch info getExitFlagPausedAndIdle getExitFlagIdle getExitFlagInited threadData2) exitBranchData2
    rhsInitedExitBranches1 <- Monad.mapM (createRhsExitBranch info getExitFlagPausedAndInited getExitFlagInited getExitFlagIdle threadData2) exitBranchData2
    rhsInitedExitBranches2 <- Monad.mapM (createRhsExitBranch info getExitFlagPausedAndInited getExitFlagInited getExitFlagInited threadData2) exitBranchData2
    
    return (TExprLinResult { lrBranches = Set.fromList (concat [ lhsUninitedNonExitBranches
                                                               , lhsInitedNonExitBranches1
                                                               , lhsInitedNonExitBranches2
                                                               , lhsUninitedExitBranches
                                                               , lhsInitedExitBranches
                                                               , lhsInitedSemiExitBranches
                                                               , rhsUninitedNonExitBranches
                                                               , rhsInitedNonExitBranches
                                                               , rhsUninitedExitBranches1
                                                               , rhsUninitedExitBranches2
                                                               , rhsInitedExitBranches1
                                                               , rhsInitedExitBranches2
                                                               ])
                           , lrParams = initFlag1 : initFlag2 : newVidDecls
                           , lrPredefInits = Map.fromList [(initFlag1, cstrFalse), (initFlag2, cstrInt getExitFlagIdle)]
                           })
linearize _ _ bexpr = error ("Behavioral expression not accounted for (\"" ++ TxsShow.fshow bexpr ++ "\")!")

createLhsNonExitBranch :: Info -> Integer -> Integer -> ThreadData -> BranchData -> IOC.IOC TxsDefs.BExpr
createLhsNonExitBranch info@(_createProcInst, initFlag1, initFlag2, _newVidDecls, g) initFlagValue1 initFlagValue2 td bd = do
    let newGuard = ValExpr.cstrAnd (Set.fromList [cstrIntEq initFlagValue1 (ValExpr.cstrVar initFlag1), cstrIntEq initFlagValue2 (ValExpr.cstrVar initFlag2), g])
    let newActOffer = addActOfferConjunct (bActOffer bd) newGuard
    let newProcInst = createNewProcInst info getExitFlagInited initFlagValue2 bd
    createBranch newActOffer newProcInst initFlagValue1 td bd
-- createLhsNonExitBranch

createLhsExitBranch :: Info -> Integer -> Integer -> ThreadData -> BranchData -> IOC.IOC TxsDefs.BExpr
createLhsExitBranch info@(_createProcInst, initFlag1, initFlag2, _newVidDecls, g) initFlagValue1 initFlagValue2 td bd = do
    let newGuard = ValExpr.cstrAnd (Set.fromList [cstrIntEq initFlagValue1 (ValExpr.cstrVar initFlag1), cstrIntEq initFlagValue2 (ValExpr.cstrVar initFlag2), g])
    let newActOffer = removeChanFromActOffer (addActOfferConjunct (bActOffer bd) newGuard) chanIdExit
    let newProcInst = createNewProcInst info getExitFlagInited getExitFlagCancelled bd
    createBranch newActOffer newProcInst initFlagValue1 td bd
-- createLhsExitBranch

createRhsNonExitBranch :: Info -> Integer -> Integer -> Integer -> ThreadData -> BranchData -> IOC.IOC TxsDefs.BExpr
createRhsNonExitBranch info@(_createProcInst, initFlag1, initFlag2, _newVidDecls, g) initFlagValue1 nextInitFlagValue1 initFlagValue2 td bd = do
    let newGuard = ValExpr.cstrAnd (Set.fromList [cstrIntEq initFlagValue1 (ValExpr.cstrVar initFlag1), cstrIntEq initFlagValue2 (ValExpr.cstrVar initFlag2), g])
    let newActOffer = addActOfferConjunct (bActOffer bd) newGuard
    let newProcInst = createNewProcInst info nextInitFlagValue1 getExitFlagInited bd
    createBranch newActOffer newProcInst initFlagValue2 td bd
-- createRhsNonExitBranch

createRhsExitBranch :: Info -> Integer -> Integer -> Integer -> ThreadData -> BranchData -> IOC.IOC TxsDefs.BExpr
createRhsExitBranch info@(_createProcInst, initFlag1, initFlag2, _newVidDecls, g) initFlagValue1 nextInitFlagValue1 initFlagValue2 td bd = do
    let newGuard = ValExpr.cstrAnd (Set.fromList [cstrIntEq initFlagValue1 (ValExpr.cstrVar initFlag1), cstrIntEq initFlagValue2 (ValExpr.cstrVar initFlag2), g])
    let newActOffer = removeChanFromActOffer (addActOfferConjunct (bActOffer bd) newGuard) chanIdExit
    let newProcInst = createNewProcInst info nextInitFlagValue1 getExitFlagIdle bd
    createBranch newActOffer newProcInst initFlagValue2 td bd
-- createRhsExitBranch

-- Because LINT wants to reduce duplication so badly...:
createNewProcInst :: Info -> Integer -> Integer -> BranchData -> TxsDefs.BExpr
createNewProcInst (createProcInst, initFlag1, initFlag2, newVidDecls, _g) nextInitFlagValue1 nextInitFlagValue2 bd =
    let newParamEqsWithoutInitFlags = Map.union (bParamEqs bd) (Map.fromSet ValExpr.cstrVar (Set.fromList (initFlag1 : initFlag2 : newVidDecls))) in
    let newInitFlagValues = Map.fromList [(initFlag1, cstrInt nextInitFlagValue1), (initFlag2, cstrInt nextInitFlagValue2)] in
    let newParamEqs = Map.union newInitFlagValues newParamEqsWithoutInitFlags in
      createProcInst (map (newParamEqs Map.!) (initFlag1 : initFlag2 : newVidDecls))
-- createNewProcInst

-- Because LINT wants to reduce duplication so badly...:
createBranch :: TxsDefs.ActOffer -> TxsDefs.BExpr -> Integer -> ThreadData -> BranchData -> IOC.IOC TxsDefs.BExpr
createBranch newActOffer newProcInst initFlagValue td bd = do
    let newActionPref = actionPref newActOffer newProcInst
    let applyInitEqs = if initFlagValue /= getExitFlagIdle then id else Subst.subst (Map.fromList (tInitEqs td)) Map.empty
    return (applyHide (bHidChans bd) (applyInitEqs newActionPref))
-- createBranch













