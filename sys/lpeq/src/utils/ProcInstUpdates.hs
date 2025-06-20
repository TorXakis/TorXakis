{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ProcInstUpdates
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ProcInstUpdates (
ProcInstUpdate,
ProcInstUpdateMap,
create,
createWithFreshPid,
createIdentical,
createAndApply,
showItem,
showMap,
addToMap,
apply,
applyMapToProcInst,
applyMapToBExpr
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad.State as MonadState
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ProcId
import qualified SortOf
import qualified ChanId
import qualified VarId
import BehExprDefs
import ProcIdFactory
import ValFactory

import BranchLinearityUtils
import ProcSearch

type ProcInstUpdate = (ProcId.ProcId, [Either Int TxsDefs.VExpr])
type ProcInstUpdateMap = Map.Map ProcId.ProcId ProcInstUpdate

showItem :: ProcInstUpdate -> String
showItem (pid, paramUpdates) = showProcId pid ++ " (" ++ List.intercalate "; " (map showParamUpdate paramUpdates) ++ ")"
  where
    showParamUpdate :: Either Int TxsDefs.VExpr -> String
    showParamUpdate (Left i) = "expr(" ++ show i ++ ")"
    showParamUpdate (Right v) = TxsShow.pshow v
-- showItem

showMap :: ProcInstUpdateMap -> String
showMap m = List.intercalate "\n" (map showEntry (Map.toList m))
  where
    showEntry :: (ProcId.ProcId, ProcInstUpdate) -> String
    showEntry (pid, paramUpdate) = showProcId pid ++ " -> " ++ showItem paramUpdate
-- showMap

(-->) :: ProcInstUpdate -> ProcInstUpdate -> ProcInstUpdate
(_pid1, xs1) --> (pid2, xs2) = (pid2, map f xs2)
  where
    f :: Either Int TxsDefs.VExpr -> Either Int TxsDefs.VExpr
    f (Left index) = xs1 !! index
    f (Right vexpr) = Right vexpr
-- -->

addToMap :: ProcInstUpdateMap -> ProcId.ProcId -> ProcInstUpdate -> ProcInstUpdateMap
addToMap procInstUpdateMap pid procInstUpdate =
    Map.insert pid procInstUpdate (Map.map chainUpdate procInstUpdateMap)
  where
    chainUpdate :: ProcInstUpdate -> ProcInstUpdate
    chainUpdate (p, u) | p /= pid = (p, u)
    chainUpdate (p, u) = (p, u) --> procInstUpdate
-- addToMap

create :: ProcId.ProcId -> [VarId.VarId] -> [VarId.VarId] -> Map.Map VarId.VarId TxsDefs.VExpr -> IOC.IOC ProcInstUpdate
create newPid oldVars newVars predefInits = do
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    return (newPid, map (f tdefs) newVars)
  where
    f :: TxsDefs.TxsDefs -> VarId.VarId -> Either Int TxsDefs.VExpr
    f tdefs vid =
        case List.elemIndex vid oldVars of
          Just i -> Left i
          Nothing -> case predefInits Map.!? vid of
                       Just v -> Right v
                       Nothing -> Right (sort2defaultValue tdefs (SortOf.sortOf vid))
-- create

createWithFreshPid :: ProcId.ProcId -> [VarId.VarId] -> [VarId.VarId] -> Map.Map VarId.VarId TxsDefs.VExpr -> IOC.IOC ProcInstUpdate
createWithFreshPid oldPid oldVars newVars predefInits = do
    newPid <- createFreshProcIdWithDifferentVars oldPid (map SortOf.sortOf newVars)
    create newPid oldVars newVars predefInits
-- createWithFreshPid

createIdentical :: ProcId.ProcId -> ProcInstUpdate
createIdentical pid = (pid, map Left [0..length (ProcId.procvars pid) - 1])

createAndApply :: ProcId.ProcId -> ProcId.ProcId -> [VarId.VarId] -> TExprLinResult -> IOC.IOC (Set.Set TxsDefs.BExpr)
createAndApply oldPid newPid newVars linResult = do
    upd <- create newPid (lrParams linResult) newVars (lrPredefInits linResult)
    return (Set.map (applyMapToBExpr (Map.singleton oldPid upd)) (lrBranches linResult))
-- createAndApply

apply :: ProcInstUpdate -> [ChanId.ChanId] -> [TxsDefs.VExpr] -> TxsDefs.BExpr
apply (newPid, paramUpdates) cids vexprs = procInst newPid cids (map f paramUpdates)
  where
    f :: Either Int TxsDefs.VExpr -> TxsDefs.VExpr
    f (Left i) = vexprs !! i
    f (Right v) = v
-- apply

applyMapToProcInst :: ProcInstUpdates.ProcInstUpdateMap -> TxsDefs.BExpr -> TxsDefs.BExpr
applyMapToProcInst procInstUpdateMap (TxsDefs.view -> ProcInst pid cids vexprs) =
    case procInstUpdateMap Map.!? pid of
      Just procInstUpdate -> apply procInstUpdate cids vexprs
      Nothing -> error ("Process not found in map (\"" ++ showProcId pid ++ "\"; map = " ++ showMap procInstUpdateMap ++ ")!")
applyMapToProcInst _procInstUpdateMap currentBExpr = error ("Process instantiation expected, but found (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")

applyMapToBExpr :: ProcInstUpdates.ProcInstUpdateMap -> TxsDefs.BExpr -> TxsDefs.BExpr
applyMapToBExpr procInstUpdateMap currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst {}) ->
          applyMapToProcInst procInstUpdateMap currentBExpr
      (TxsDefs.view -> Guard g bexpr) ->
          guard g (applyMapToBExpr procInstUpdateMap bexpr)
      (TxsDefs.view -> Choice bexprs) ->
          choice (Set.map (applyMapToBExpr procInstUpdateMap) bexprs)
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          parallel cidSet (map (applyMapToBExpr procInstUpdateMap) bexprs)
      (TxsDefs.view -> Hide cidSet bexpr) ->
          hide cidSet (applyMapToBExpr procInstUpdateMap bexpr)
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          enable (applyMapToBExpr procInstUpdateMap bexpr1) acceptOffers (applyMapToBExpr procInstUpdateMap bexpr2)
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          disable (applyMapToBExpr procInstUpdateMap bexpr1) (applyMapToBExpr procInstUpdateMap bexpr2)
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          interrupt (applyMapToBExpr procInstUpdateMap bexpr1) (applyMapToBExpr procInstUpdateMap bexpr2)
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          actionPref actOffer (applyMapToBExpr procInstUpdateMap bexpr)
      (TxsDefs.view -> ValueEnv _venv _bexpr) ->
          error ("ValueEnv should have been eliminated by now (\"" ++ show currentBExpr ++ "\")!")
      (TxsDefs.view -> StAut _sid _venv _transitions) ->
          error ("StAut should have been eliminated by now (\"" ++ show currentBExpr ++ "\")!")
-- applyMapToBExpr





