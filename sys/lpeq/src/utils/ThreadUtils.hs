{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ThreadUtils
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ThreadUtils (
SubExprType(..),
getSubExprType,
BranchData(..),
emptyBranchData,
getBranchData,
ThreadData(..),
getThreadData,
filterThreadData,
partitionThreadData
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ChanId
import qualified VarId
import qualified SortId
import ActOfferFactory
import VarFactory
import BehExprDefs

import BranchLinearityUtils

data SubExprType = StpSequential    -- Behavioral expression that does not known to occur after an action.
                 | StpPrefixed      -- Behavioral expression that is known to occur after an action.
                 | StpStackable     -- Parent process can be instantiated in this sub-expression iff there is a stack that keeps track of the parameters of each instance.
                 | StpUnsafe        -- Parent process cannot be recursively instantiated in this sub-expression (to avoid infinite recursion).
                   deriving (Eq, Ord, Show)

getSubExprType :: TxsDefs.BExpr -> Int -> SubExprType
getSubExprType currentBExpr subExprIndex =
    case currentBExpr of
      (TxsDefs.view -> ProcInst {}) -> StpSequential
      (TxsDefs.view -> Guard {}) -> StpSequential
      (TxsDefs.view -> Choice {}) -> StpSequential
      (TxsDefs.view -> Parallel {}) -> StpUnsafe
      (TxsDefs.view -> Hide {}) -> StpSequential
      (TxsDefs.view -> Enable {}) -> if subExprIndex == 0 then StpStackable else StpPrefixed
      (TxsDefs.view -> Disable {}) -> if subExprIndex == 0 then StpUnsafe else StpPrefixed
      (TxsDefs.view -> Interrupt {}) -> if subExprIndex == 0 then StpUnsafe else StpStackable
      (TxsDefs.view -> ActionPref {}) -> StpPrefixed
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- getSubExprType

data BranchData = BranchData { bOrigExpr :: TxsDefs.BExpr
                             , bHidChans :: Set.Set ChanId.ChanId
                             , bVizChans :: Set.Set ChanId.ChanId
                             , bActOffer :: ActOffer
                             , bParamEqs :: Map.Map VarId.VarId TxsDefs.VExpr
                             , bOfferVarsPerChan :: Map.Map ChanId.ChanId [VarId.VarId]
                             } deriving (Eq, Ord, Show)
-- BranchData

emptyBranchData :: BranchData
emptyBranchData = BranchData { bOrigExpr = stop
                             , bHidChans = Set.empty
                             , bVizChans = Set.empty
                             , bActOffer = emptyActOffer
                             , bParamEqs = Map.empty
                             , bOfferVarsPerChan = Map.empty
                             }
-- emptyBranchData

getBranchData :: TxsDefs.BExpr -> IOC.IOC BranchData
getBranchData bexpr = do
    let (hiddenChans, actOffer, recProcInst) = getBranchSegments bexpr
    (_branches, paramEqs) <- extractProcInstData recProcInst
    let offerVarsPerChan = getOfferVarsPerChan actOffer
    return (BranchData { bOrigExpr = bexpr
                       , bHidChans = hiddenChans
                       , bVizChans = getActOfferChans actOffer
                       , bActOffer = actOffer
                       , bParamEqs = Map.fromList paramEqs
                       , bOfferVarsPerChan = offerVarsPerChan
                       })
-- getBranchData

data ThreadData = ThreadData { tBranchData :: Set.Set BranchData            -- (Data about the) branches of the thread to which the ProcInst refers.
                             , tInitVar :: VarId.VarId                      -- Flag that indicates whether the thread has been initialized.
                             , tInitEqs :: [(VarId.VarId, TxsDefs.VExpr)]   -- Equations with which the thread should be initialized.
                             } deriving (Eq, Ord, Show)
-- ThreadData

-- Extracts data from a thread (which must have the form of a process instantiation).
-- Also creates an initialization flag for the thread.
getThreadData :: String -> SortId.SortId -> TxsDefs.BExpr -> IOC.IOC ThreadData
getThreadData initVarPrefix initVarSort bexpr = do
    (branches, initEqs) <- extractProcInstData bexpr
    branchData <- Set.fromList <$> Monad.mapM getBranchData (Set.toList branches)
    initVar <- createFreshVarFromPrefix initVarPrefix initVarSort
    return (ThreadData { tBranchData = branchData, tInitVar = initVar, tInitEqs = initEqs })
-- getThreadData

filterThreadData :: (Set.Set ChanId.ChanId -> Bool) -> ThreadData -> ThreadData
filterThreadData restrictionFunction threadData =
    threadData { tBranchData = Set.filter (restrictionFunction . bVizChans) (tBranchData threadData) }
-- filterThreadData

partitionThreadData :: (Set.Set ChanId.ChanId -> Bool) -> ThreadData -> (ThreadData, ThreadData)
partitionThreadData restrictionFunction threadData =
    let (p, q) = Set.partition (restrictionFunction . bVizChans) (tBranchData threadData) in
      (threadData { tBranchData = p }, threadData { tBranchData = q })
-- partitionThreadData



















