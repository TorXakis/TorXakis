{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TExprLinearization
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module TExprLinearization (
linearizeTExprs
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ProcId
import qualified ProcDef
import qualified SortOf
import BehExprDefs
import ProcIdFactory
import ProcDepTree

import qualified ProcInstUpdates

import HideElim
import BranchLinearityUtils
import PrefixResolution
import ProcSearch

import qualified LinearizeParallel
import qualified LinearizeEnable
import qualified LinearizeDisable
import qualified LinearizeInterrupt

linearizeTExprs :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
linearizeTExprs bexpr = do
    procDepTree <- getProcDepTree bexpr
    let orderedProcs = getProcsOrderedByMaxDepth procDepTree
    procInstUpdateMap <- Monad.foldM linearizeTExprsInProc Map.empty orderedProcs
    return (ProcInstUpdates.applyMapToProcInst procInstUpdateMap bexpr)
-- linearizeTExprs

linearizeTExprsInProc :: ProcInstUpdates.ProcInstUpdateMap -> ProcId.ProcId -> IOC.IOC ProcInstUpdates.ProcInstUpdateMap
linearizeTExprsInProc procInstUpdateMap pid = do
    IOC.putInfo [ "Linearizing " ++ showProcId pid ++ "..." ]
    -- printProcsInBody ("Current form of " ++ showProcId pid ++ " ====>") pid
    r <- getProcById pid
    case r of
      Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
          -- Function to be used for the instantiation of the linearized process:
          let createProcInst = procInst pid cidDecls
          
          -- Distinguish linear branches in the body that are finished from non-linear branches (=branches with thread expressions):
          let (nlbranches, lbranches) = Set.partition isNonLinearBranch (getBranches body)
          
          -- Linearize non-linear branches:
          let tempProcInstUpdateMap = ProcInstUpdates.addToMap procInstUpdateMap pid (ProcInstUpdates.createIdentical pid)
          rs <- Monad.mapM (linearizeTExpr createProcInst tempProcInstUpdateMap) (Set.toList nlbranches)
          
          -- Check if the result is linear. IT SHOULD BE LINEAR!
          checkLinearBExprs pid (Set.toList nlbranches) (Set.toList (Set.unions (map lrBranches rs)))
          
          let newVids = concatMap lrParams rs
          let newVidDecls = vidDecls ++ newVids
          let newPredefInits = Map.unions (map lrPredefInits rs)
          
          -- Replace process instantiations in branches that were just linearized.
          -- (Currently, they probably are incorrect because they only set newly introduced variables.)
          newProcId <- createFreshProcIdWithDifferentVars pid (map SortOf.sortOf newVidDecls)
          newPBranches <- Set.unions <$> Monad.mapM (ProcInstUpdates.createAndApply pid newProcId newVidDecls) rs
          
          -- Replace instantiations of the current process in branches that were already finished.
          -- Remember how such instantiations should be updated, for later use.
          newProcInstUpdate <- ProcInstUpdates.create newProcId vidDecls newVidDecls newPredefInits
          let newProcInstUpdateMap = ProcInstUpdates.addToMap procInstUpdateMap pid newProcInstUpdate
          let newNPBranches = Set.map (ProcInstUpdates.applyMapToBExpr newProcInstUpdateMap) lbranches
          
          -- Newly produced branches may not have an action prefix.
          -- Therefore, do another round of prefix resolution, and
          -- register the process with a new body constructed from the new branches.
          let newBranches = Set.union newNPBranches newPBranches
          newBody <- resolveProcPrefixesInBody newProcId cidDecls newVidDecls (choice newBranches)
          registerProc newProcId (ProcDef.ProcDef cidDecls newVidDecls newBody)
          
          -- Check if the result is linear. IT SHOULD BE LINEAR!
          checkLinearBExprs newProcId (Set.toList newBranches) (Set.toList (getBranches newBody))
          
          (_newProcId', newProcInstUpdateMap') <- eliminateHide (newProcId, newProcInstUpdateMap)
          -- printProcsInBody "AFTER HIDE ------> " newProcId'
          return newProcInstUpdateMap'
      Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
-- linearizeTExprsInProc

linearizeTExpr :: ([TxsDefs.VExpr] -> TxsDefs.BExpr) -> ProcInstUpdates.ProcInstUpdateMap -> TxsDefs.BExpr -> IOC.IOC TExprLinResult
linearizeTExpr createProcInst procInstUpdateMap currentBExpr =
    let remappedBExpr = ProcInstUpdates.applyMapToBExpr procInstUpdateMap currentBExpr in
      case remappedBExpr of
        (TxsDefs.view -> Hide cidSet bexpr) -> do linResult <- linearizeNonHideTExpr createProcInst bexpr
                                                  return (linResult { lrBranches = Set.map (applyHide cidSet) (lrBranches linResult) })
        _ -> linearizeNonHideTExpr createProcInst remappedBExpr
-- linearizeTExpr

linearizeNonHideTExpr :: ([TxsDefs.VExpr] -> TxsDefs.BExpr) -> TxsDefs.BExpr -> IOC.IOC TExprLinResult
linearizeNonHideTExpr createProcInst currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> Guard g bexpr) ->
          case bexpr of
            (TxsDefs.view -> Parallel {}) -> LinearizeParallel.linearize createProcInst g bexpr
            (TxsDefs.view -> Enable {}) -> LinearizeEnable.linearize createProcInst g bexpr
            (TxsDefs.view -> Disable {}) -> LinearizeDisable.linearize createProcInst g bexpr
            (TxsDefs.view -> Interrupt {}) -> LinearizeInterrupt.linearize createProcInst g bexpr
            _ -> error ("No implementation yet for \"" ++ show currentBExpr ++ "\"!")
      _ -> error ("Behavioral expression not accounted for (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- linearizeNonHideTExpr





