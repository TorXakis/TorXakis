{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  UniqueObjects
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module UniqueObjects (
ensureFreshProcsInBExpr,
ensureDistinguishableThreads,
ensureFreshVarsInProcInst,
ensureFreshVarsInBranch,
ensureFreshVarsInBExpr
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ValExpr
import qualified VarId
import qualified Subst
import qualified ProcId
import qualified ProcDef
import BehExprDefs
import ActOfferFactory
import ProcIdFactory
import VarFactory

import BranchUtils
import ProcSearch

-- Creates copies of all processes reachable from the given behavioral expression.
-- Updates process instantiations (in the copies) accordingly.
-- Returns the original behavioral expression in which process instantiations have been updated.
ensureFreshProcsInBExpr :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
ensureFreshProcsInBExpr bexpr = do
    procIds <- getProcsInBExpr bexpr
    freshPidPerPid <- Monad.mapM clonePid (Set.toList procIds)
    let freshPidMap = Map.fromList freshPidPerPid
    Monad.mapM_ (doProc freshPidMap) freshPidPerPid
    replacePidsInBExpr freshPidMap bexpr
  where
    clonePid :: TxsDefs.ProcId -> IOC.IOC (TxsDefs.ProcId, TxsDefs.ProcId)
    clonePid pid = do
        freshPid <- createFreshProcIdFromProcId pid
        return (pid, freshPid)
    -- clonePid
    
    doProc :: Map.Map TxsDefs.ProcId TxsDefs.ProcId -> (TxsDefs.ProcId, TxsDefs.ProcId) -> IOC.IOC ()
    doProc freshPidMap (pid, freshPid) = do
        r <- getProcById pid
        case r of
          Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
              body' <- replacePidsInBExpr freshPidMap body
              registerProc freshPid (ProcDef.ProcDef cidDecls vidDecls body')
          Nothing -> return ()
    -- doProc
-- ensureFreshProcsInBExpr

replacePidsInBExpr :: Map.Map TxsDefs.ProcId TxsDefs.ProcId -> TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
replacePidsInBExpr freshPidMap currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid cids vexprs) ->
          return (procInst (freshPidMap Map.! pid) cids vexprs)
      (TxsDefs.view -> Guard g bexpr) ->
          do bexpr' <- replacePidsInBExpr freshPidMap bexpr
             return (guard g bexpr')
      (TxsDefs.view -> Choice bexprs) ->
          do bexprs' <- Set.fromList <$> Monad.mapM (replacePidsInBExpr freshPidMap) (Set.toList bexprs)
             return (choice bexprs')
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          do bexprs' <- Monad.mapM (replacePidsInBExpr freshPidMap) bexprs
             return (parallel cidSet bexprs')
      (TxsDefs.view -> Hide cidSet bexpr) ->
          do bexpr' <- replacePidsInBExpr freshPidMap bexpr
             return (hide cidSet bexpr')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          do bexpr1' <- replacePidsInBExpr freshPidMap bexpr1
             bexpr2' <- replacePidsInBExpr freshPidMap bexpr2
             return (enable bexpr1' acceptOffers bexpr2')
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do bexpr1' <- replacePidsInBExpr freshPidMap bexpr1
             bexpr2' <- replacePidsInBExpr freshPidMap bexpr2
             return (disable bexpr1' bexpr2')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do bexpr1' <- replacePidsInBExpr freshPidMap bexpr1
             bexpr2' <- replacePidsInBExpr freshPidMap bexpr2
             return (interrupt bexpr1' bexpr2')
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          do bexpr' <- replacePidsInBExpr freshPidMap bexpr
             return (actionPref actOffer bexpr')
      (TxsDefs.view -> ValueEnv _venv bexpr) ->
          replacePidsInBExpr freshPidMap bexpr
      -- (TxsDefs.view -> StAut _sid _venv transitions) -> 
          -- ...
      _ -> error ("Behavioral expression not anticipated (\"" ++ show currentBExpr ++ "\")!")
-- replacePidsInBExpr

ensureDistinguishableThreads :: [TxsDefs.BExpr] -> IOC.IOC [TxsDefs.BExpr]
ensureDistinguishableThreads threads = do
    (newThreads, _) <- Monad.foldM addThread ([], Set.empty) threads
    return newThreads
  where
    addThread :: ([TxsDefs.BExpr], Set.Set ProcId.ProcId) -> TxsDefs.BExpr -> IOC.IOC ([TxsDefs.BExpr], Set.Set ProcId.ProcId)
    addThread (soFar, visitedProcs) bexpr@(TxsDefs.view -> ProcInst pid cids vexprs) =
        if Set.member pid visitedProcs
        then do r <- getProcById pid
                case r of
                  Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
                      freshPid <- createFreshProcIdFromProcId pid
                      body' <- replacePidsInBExpr (Map.singleton pid freshPid) body
                      registerProc freshPid (ProcDef.ProcDef cidDecls vidDecls body')
                      return (soFar ++ [procInst freshPid cids vexprs], Set.insert freshPid visitedProcs)
                  Nothing -> error ("Unknown process (\"" ++ showProcId pid ++ "\")!")
        else return (soFar ++ [bexpr], Set.insert pid visitedProcs)
    addThread _ bexpr = error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow bexpr ++ "\")!")
-- ensureDistinguishableThreads

ensureFreshVarsInProcInst :: TxsDefs.BExpr -> IOC.IOC ()
ensureFreshVarsInProcInst (TxsDefs.view -> ProcInst pid _cids _vexprs) = do
    r <- getProcById pid
    case r of
      Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
          subst <- createFreshVars (Set.fromList vidDecls)
          newBranches <- Monad.mapM (ensureFreshVarsInBranch subst) (Set.toList (getBranches body))
          let body' = choice (Set.fromList newBranches)
          registerProc pid (ProcDef.ProcDef cidDecls (map (subst Map.!) vidDecls) body')
      Nothing -> error ("Unknown process (\"" ++ showProcId pid ++ "\")!")
ensureFreshVarsInProcInst currentBExpr = error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")

ensureFreshVarsInBranch :: Map.Map VarId.VarId VarId.VarId -> TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
ensureFreshVarsInBranch subst currentBExpr = do
    let (nonHiddenBExpr, hiddenChans) = removeHide currentBExpr
    case nonHiddenBExpr of
      (TxsDefs.view -> ActionPref actOffer bexpr) -> do
          actOfferSubst <- createFreshVars (getActOfferVarSet actOffer)
          let subst' = Map.union actOfferSubst subst
          let actOffer' = replaceVarsInActOffer subst' actOffer
          let bexpr' = Subst.subst (Map.map ValExpr.cstrVar subst') Map.empty bexpr
          let actionPref' = actionPref actOffer' bexpr'
          return (applyHide hiddenChans actionPref')
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- ensureFreshVarsInBranch

ensureFreshVarsInBExpr :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
ensureFreshVarsInBExpr bexpr = do
    procIds <- getProcsInBExpr bexpr
    Monad.foldM_ doProc () procIds -- TODO change to mapM_
    return bexpr
  where
    doProc :: () -> TxsDefs.ProcId -> IOC.IOC ()
    doProc _ pid = do
        r <- getProcById pid
        case r of
          Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
              subst <- createFreshVars (Set.fromList vidDecls)
              body' <- replaceVidsInBExpr subst body
              registerProc pid (ProcDef.ProcDef cidDecls (map (subst Map.!) vidDecls) body')
          Nothing -> return ()
    -- doProc
-- ensureFreshVarsInBExpr

replaceVidsInBExpr :: Map.Map VarId.VarId VarId.VarId -> TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
replaceVidsInBExpr subst currentBExpr = do
    let applySubst f = Subst.subst (Map.map ValExpr.cstrVar f) Map.empty
    case currentBExpr of
      (TxsDefs.view -> ProcInst {}) ->
          return (applySubst subst currentBExpr)
      (TxsDefs.view -> Guard g bexpr) ->
          do bexpr' <- replaceVidsInBExpr subst bexpr
             return (guard (applySubst subst g) bexpr')
      (TxsDefs.view -> Choice bexprs) ->
          do bexprs' <- Set.fromList <$> Monad.mapM (replaceVidsInBExpr subst) (Set.toList bexprs)
             return (choice bexprs')
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          do bexprs' <- Monad.mapM (replaceVidsInBExpr subst) bexprs
             return (parallel cidSet bexprs')
      (TxsDefs.view -> Hide cidSet bexpr) ->
          do bexpr' <- replaceVidsInBExpr subst bexpr
             return (hide cidSet bexpr')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          do acceptOfferSubst <- createFreshVars (Set.fromList (map getChanOfferVar acceptOffers))
             let subst' = Map.union acceptOfferSubst subst
             bexpr1' <- replaceVidsInBExpr subst' bexpr1
             bexpr2' <- replaceVidsInBExpr subst' bexpr2
             return (enable bexpr1' (map (applySubst subst') acceptOffers) bexpr2')
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do bexpr1' <- replaceVidsInBExpr subst bexpr1
             bexpr2' <- replaceVidsInBExpr subst bexpr2
             return (disable bexpr1' bexpr2')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do bexpr1' <- replaceVidsInBExpr subst bexpr1
             bexpr2' <- replaceVidsInBExpr subst bexpr2
             return (interrupt bexpr1' bexpr2')
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          do actOfferSubst <- createFreshVars (getActOfferVarSet actOffer)
             let subst' = Map.union actOfferSubst subst
             bexpr' <- replaceVidsInBExpr subst' bexpr
             return (actionPref (replaceVarsInActOffer subst' actOffer) bexpr')
      (TxsDefs.view -> ValueEnv venv bexpr) ->
          do venvSubst <- createFreshVars (Map.keysSet venv)
             let subst' = Map.union venvSubst subst
             bexpr' <- replaceVidsInBExpr subst' bexpr
             let applySubstToVEnv ve = Map.fromList (map (\(k, v) -> (subst' Map.! k, applySubst subst' v)) (Map.toList ve))
             return (valueEnv (applySubstToVEnv venv) bexpr')
      -- (TxsDefs.view -> StAut _sid _venv transitions) -> 
          -- ...
      _ -> error ("Behavioral expression not anticipated (\"" ++ show currentBExpr ++ "\")!")
-- replaceVidsInBExpr






