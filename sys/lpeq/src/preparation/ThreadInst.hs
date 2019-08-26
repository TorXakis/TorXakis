{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ThreadInst
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ThreadInst (
doThreadInst
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Control.Monad as Monad
import qualified Control.Monad.State as MonadState
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ValExpr
import qualified ProcId
import qualified ProcDef
import qualified ChanId
import BehExprDefs
import ProcIdFactory
import qualified Scopes

import ProcSearch
import ThreadUtils

-- `Threads' are sub-expressions of Parallel / Enable / Disable / Interrupt expressions.
-- This function recursively identifies such sub-expressions in a given behavioral expression, and
-- creates process definitions (=thread processes) for each of them.
-- Threads are replaced by instantiations of corresponding process definitions.
-- 
-- The given behavioral expression should be closed except for channels, of which a list must be provided.
doThreadInst :: [ChanId.ChanId] -> TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
doThreadInst allChanIds startBExpr = do
    procIds <- getProcsInBExpr startBExpr
    Monad.mapM_ doProc procIds
    (bexpr, _exit) <- instThread "RootProc" (Scopes.fromDecls allChanIds []) startBExpr
    return bexpr -- Maybe check if EXIT has correct type?
  where
    doProc :: ProcId.ProcId -> IOC.IOC ()
    doProc pid = do
        r <- getProcById pid
        case r of
          Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
              -- IOC.putInfo [ "doThreadInst " ++ TxsShow.fshow pid ]
              (body', _exit) <- lookForThread (Text.unpack (ProcId.name pid)) (Scopes.fromDecls cidDecls vidDecls) body
              registerProc pid (ProcDef.ProcDef cidDecls vidDecls body')
          Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
-- doThreadInst

-- Searches a given expression for parallel sub-expressions.
lookForThread :: String -> Scopes.Scope -> TxsDefs.BExpr -> IOC.IOC (TxsDefs.BExpr, ProcId.ExitSort)
lookForThread location scope currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid cids vexprs) ->
          return (procInst pid (Scopes.applyToChans scope cids) (Scopes.applyToVExprs scope vexprs), ProcId.procexit pid)
      (TxsDefs.view -> Choice bexprs) ->
          do (bexprs', exit') <- forAllBExprs (lookForThread location scope) (Set.toList bexprs)
             return (choice (Set.fromList bexprs'), exit')
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          do (bexprs', exit') <- forAllBExprs (instThread location scope) bexprs
             return (parallel (Scopes.applyToChanSet scope cidSet) bexprs', exit')
      (TxsDefs.view -> Guard g bexpr) ->
          do (bexpr', exit') <- handleSubExpr currentBExpr 0 location scope bexpr
             return (guard (Scopes.applyToVExpr scope g) bexpr', exit')
      (TxsDefs.view -> Hide cidSet bexpr) ->
          do let scope' = Scopes.addChanSet scope cidSet
             (bexpr', exit') <- handleSubExpr currentBExpr 0 location scope' bexpr
             return (hide (Scopes.applyToChanSet scope' cidSet) bexpr', exit')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          do (bexpr1', _) <- handleSubExpr currentBExpr 0 location scope bexpr1
             (bexpr2', exit') <- handleSubExpr currentBExpr 1 location (Scopes.addChanOffers scope acceptOffers) bexpr2
             return (enable bexpr1' (map (Scopes.applyToChanOffer scope) acceptOffers) bexpr2', exit')
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do (bexpr1', _) <- handleSubExpr currentBExpr 0 location scope bexpr1
             (bexpr2', exit') <- handleSubExpr currentBExpr 1 location scope bexpr2
             return (disable bexpr1' bexpr2', exit')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do (bexpr1', _) <- handleSubExpr currentBExpr 0 location scope bexpr1
             (bexpr2', exit') <- handleSubExpr currentBExpr 1 location scope bexpr2
             return (interrupt bexpr1' bexpr2', exit')
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          do let scope' = Scopes.addActOffer scope actOffer
             (bexpr', exit') <- handleSubExpr currentBExpr 0 location scope' bexpr
             return (actionPref (Scopes.applyToActOffer scope' actOffer) bexpr', exit')
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- lookForThread

instThread :: String -> Scopes.Scope -> TxsDefs.BExpr -> IOC.IOC (TxsDefs.BExpr, ProcId.ExitSort)
instThread location scope currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid cids vexprs) ->
          return (procInst pid (Scopes.applyToChans scope cids) (Scopes.applyToVExprs scope vexprs), ProcId.procexit pid)
      (TxsDefs.view -> Choice bexprs) ->
          do scope' <- Scopes.cloneScope scope
             (bexprs', exit') <- forAllBExprs (lookForThread location scope') (Set.toList bexprs)
             regAndInstProc location scope' exit' (choice (Set.fromList bexprs'))
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          do scope' <- Scopes.cloneScope scope
             (bexprs', exit') <- forAllBExprs (instThread location scope') bexprs
             regAndInstProc location scope' exit' (parallel (Scopes.applyToChanSet scope' cidSet) bexprs')
      (TxsDefs.view -> Guard g bexpr) ->
          do scope' <- Scopes.cloneScope scope
             (bexpr', exit') <- handleSubExpr currentBExpr 0 location scope' bexpr
             regAndInstProc location scope' exit' (guard (Scopes.applyToVExpr scope' g) bexpr')
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          do scope' <- Scopes.cloneScope scope
             let scope'' = Scopes.addActOffer scope' actOffer
             (bexpr', exit') <- handleSubExpr currentBExpr 0 location scope'' bexpr
             regAndInstProc location scope' exit' (actionPref (Scopes.applyToActOffer scope'' actOffer) bexpr')
      (TxsDefs.view -> Hide cidSet bexpr) ->
          do scope' <- Scopes.cloneScope scope
             let scope'' = Scopes.addChanSet scope' cidSet
             (bexpr', exit') <- handleSubExpr currentBExpr 0 location scope'' bexpr
             regAndInstProc location scope'' exit' (hide (Scopes.applyToChanSet scope'' cidSet) bexpr')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          do scope1 <- Scopes.cloneScope scope
             (bexpr1', _) <- handleSubExpr currentBExpr 0 location scope1 bexpr1
             let scope2 = Scopes.addChanOffers scope1 acceptOffers
             (bexpr2', exit') <- handleSubExpr currentBExpr 1 location scope2 bexpr2
             regAndInstProc location scope1 exit' (enable bexpr1' (map (Scopes.applyToChanOffer scope2) acceptOffers) bexpr2')
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do (bexpr1', bexpr2', exit', scope') <- cloneScopeAndInstThread bexpr1 bexpr2
             regAndInstProc location scope' exit' (disable bexpr1' bexpr2')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do (bexpr1', bexpr2', exit', scope') <- cloneScopeAndInstThread bexpr1 bexpr2
             regAndInstProc location scope' exit' (interrupt bexpr1' bexpr2')
      _ -> error ("Behavioral expression not accounted for (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
  where
    -- Because LINT wants to reduce duplication so badly...:
    cloneScopeAndInstThread :: TxsDefs.BExpr -> TxsDefs.BExpr -> IOC.IOC (TxsDefs.BExpr, TxsDefs.BExpr, ProcId.ExitSort, Scopes.Scope)
    cloneScopeAndInstThread bexpr1 bexpr2 = do
        scope' <- Scopes.cloneScope scope
        (bexpr1', _) <- handleSubExpr currentBExpr 0 location scope' bexpr1
        (bexpr2', exit') <- handleSubExpr currentBExpr 1 location scope' bexpr2
        return (bexpr1', bexpr2', exit', scope')
    -- cloneScopeAndInstThread
-- instThread

-- Determines whether a behavioral expression should be instantiated or not.
handleSubExpr :: TxsDefs.BExpr -> Int -> String -> Scopes.Scope -> TxsDefs.BExpr -> IOC.IOC (TxsDefs.BExpr, ProcId.ExitSort)
handleSubExpr currentBExpr subExprIndex =
    case getSubExprType currentBExpr subExprIndex of
      StpSequential -> lookForThread
      StpPrefixed -> lookForThread
      StpStackable -> instThread
      StpUnsafe -> instThread
-- handleSubExpr

-- Multiple branches are evaluated in the same manner with this function.
forAllBExprs :: (TxsDefs.BExpr -> IOC.IOC (TxsDefs.BExpr, ProcId.ExitSort)) -> [TxsDefs.BExpr] -> IOC.IOC ([TxsDefs.BExpr], ProcId.ExitSort)
forAllBExprs f bexprs = do
    rs <- Monad.mapM f bexprs
    if null rs
    then return ([], ProcId.NoExit)
    else return (map fst rs, last (map snd rs))
-- forAllBExprs

-- Creates a process definition from the given scope and body and registers it.
-- It also creates an instantiation of the process, which is returned.
regAndInstProc :: String -> Scopes.Scope -> ProcId.ExitSort -> TxsDefs.BExpr -> IOC.IOC (TxsDefs.BExpr, ProcId.ExitSort)
regAndInstProc location scope exit body = do
    let cids = Map.toList (Scopes.chanMap scope)
    let vids = Map.toList (Scopes.varMap scope)
    let newProcParams = map snd vids
    newPid <- createFreshProcIdFromChansAndVars (Text.pack (location ++ "Sub")) (map snd cids) newProcParams exit
    let newPDef = ProcDef.ProcDef (map snd cids) newProcParams body
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    let tdefs' = tdefs { TxsDefs.procDefs = Map.insert newPid newPDef (TxsDefs.procDefs tdefs) }
    IOC.modifyCS (\st -> st { IOC.tdefs = tdefs' })
    let newProcValues = map (ValExpr.cstrVar . fst) vids
    return (procInst newPid (map fst cids) newProcValues, exit)
-- regAndInstProcUsingVarEnv



















