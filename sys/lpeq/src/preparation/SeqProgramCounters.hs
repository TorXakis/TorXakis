{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  SeqProgramCounters
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module SeqProgramCounters (
addSeqProgramCounters
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
import qualified FreeVar
import qualified Constant
import qualified ProcId
import qualified ProcDef
import qualified VarId
import BehExprDefs
import ActOfferFactory
import ProcIdFactory
import VarFactory

import qualified ProcInstUpdates

import BranchUtils
import ProcDepTree
import ThreadUtils
import UniqueObjects

addSeqProgramCounters :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
addSeqProgramCounters bexpr = do
    bexpr' <- ensureFreshVarsInBExpr bexpr
    procDepTree <- getProcDepTree bexpr'
    let orderedProcs = getProcsOrderedByMaxDepth procDepTree
    procInstUpdateMap <- Monad.foldM addSeqPCsToProc Map.empty orderedProcs
    return (ProcInstUpdates.applyMapToProcInst procInstUpdateMap bexpr')
-- addSeqProgramCounters

addSeqPCsToProc :: ProcInstUpdates.ProcInstUpdateMap -> ProcId.ProcId -> IOC.IOC ProcInstUpdates.ProcInstUpdateMap
addSeqPCsToProc procInstUpdateMap pid = do
    IOC.putInfo [ "addSeqPCsToProc " ++ TxsShow.fshow pid ]
    r <- getProcById pid
    case r of
      Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
          seqPC <- createFreshIntVarFromPrefix "seqPC"
          extraVids <- getSeqVidDecls (Set.singleton pid) body
          let ownerVidDecls = seqPC:Set.toList extraVids
          newProcInstUpdate <- ProcInstUpdates.createWithFreshPid pid vidDecls ownerVidDecls (Map.singleton seqPC (ValExpr.cstrConst (Constant.Cint 0)))
          -- IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO ("update " ++ ProcInstUpdates.showItem newProcInstUpdate) ]
          let procInstUpdateMap' = ProcInstUpdates.addToMap procInstUpdateMap pid newProcInstUpdate
          let createProcInst = procInst (fst newProcInstUpdate) cidDecls
          (_, body', _, _) <- constructSeqBExpr procInstUpdateMap' createProcInst ownerVidDecls seqPC 0 (Map.singleton pid 0) body
          
          -- unregisterProc pid -- TODO Do this later
          registerProc (fst newProcInstUpdate) (ProcDef.ProcDef cidDecls ownerVidDecls (choice body'))
          return procInstUpdateMap'
      Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
-- addSeqPCsToProc

-- Extracts all variables that (may) need to be parameters when combining reachable processes.
getSeqVidDecls :: Set.Set ProcId.ProcId -> TxsDefs.BExpr -> IOC.IOC (Set.Set VarId.VarId)
getSeqVidDecls visitedProcs currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid _cids vexprs) -> do
          let procInstVars = Set.fromList (concatMap FreeVar.freeVars vexprs)
          if Set.member pid visitedProcs
          then return procInstVars
          else do r <- getProcById pid
                  case r of
                    -- (We ignore the declared parameters; if they are never used, we do not need to set them, anyway.)
                    Just (ProcDef.ProcDef _cidDecls _vidDecls body) ->
                        Set.union procInstVars <$> getSeqVidDecls (Set.insert pid visitedProcs) body
                    Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
      (TxsDefs.view -> Choice bexprs) ->
          Set.unions <$> Monad.mapM (getVidDecls currentBExpr 0 visitedProcs) (Set.toList bexprs)
      (TxsDefs.view -> Parallel _cidSet bexprs) ->
          Set.unions <$> Monad.mapM (getVidDecls currentBExpr 0 visitedProcs) bexprs
      (TxsDefs.view -> Guard g bexpr) -> do
          let guardVars = Set.fromList (FreeVar.freeVars g)
          Set.union guardVars <$> getVidDecls currentBExpr 0 visitedProcs bexpr
      (TxsDefs.view -> ActionPref actOffer bexpr) -> do
          let actionPrefVars = Set.fromList (FreeVar.freeVars (TxsDefs.constraint actOffer))
          Set.union actionPrefVars <$> getVidDecls currentBExpr 0 visitedProcs bexpr
      (TxsDefs.view -> Hide _cidSet bexpr) ->
          getVidDecls currentBExpr 0 visitedProcs bexpr
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) -> do
          let acceptVars = Set.fromList (map getChanOfferVar acceptOffers)
          Set.union acceptVars <$> getBinVidDecls bexpr1 bexpr2
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          getBinVidDecls bexpr1 bexpr2
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          getBinVidDecls bexpr1 bexpr2
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
  where
    getBinVidDecls :: TxsDefs.BExpr -> TxsDefs.BExpr -> IOC.IOC (Set.Set VarId.VarId)
    getBinVidDecls bexpr1 bexpr2 = do
        vidDecls1 <- getVidDecls currentBExpr 0 visitedProcs bexpr1
        vidDecls2 <- getVidDecls currentBExpr 1 visitedProcs bexpr2
        return (Set.union vidDecls1 vidDecls2)
    -- getBinVidDecls
-- getSeqVidDecls

getParVidDecls :: Set.Set ProcId.ProcId -> TxsDefs.BExpr -> IOC.IOC (Set.Set VarId.VarId)
getParVidDecls _visitedProcs currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst _pid _cids vexprs) -> return (Set.fromList (concatMap FreeVar.freeVars vexprs))
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- getParVidDecls

getVidDecls :: TxsDefs.BExpr -> Int -> Set.Set ProcId.ProcId -> TxsDefs.BExpr -> IOC.IOC (Set.Set VarId.VarId)
getVidDecls currentBExpr subExprIndex =
    case getSubExprType currentBExpr subExprIndex of
      StpSequential -> getSeqVidDecls
      StpPrefixed -> getSeqVidDecls
      StpStackable -> getParVidDecls
      StpUnsafe -> getParVidDecls
-- getVidDecls

constructSeqBExpr :: ProcInstUpdates.ProcInstUpdateMap         -- Contains information about how parallel processes on which we are dependent should be instantiated.
                  -> ([TxsDefs.VExpr] -> TxsDefs.BExpr)        -- Function that constructs a recursive process instantiation.
                  -> [VarId.VarId]                             -- All parameters of the process to which we are adding a sequential PC (including the PC).
                  -> VarId.VarId                               -- The sequential PC.
                  -> Integer                                   -- Value of the PC at the level of the current behavioral expression.
                  -> Map.Map ProcId.ProcId Integer             -- Current PC values per visited process.
                  -> TxsDefs.BExpr                             -- Current behavioral expression.
                  -> IOC.IOC ( TxsDefs.BExpr                   -- Instantiation that can replace the current behavioral expression.
                             , Set.Set TxsDefs.BExpr           -- Body constructed from the current behavioral expression.
                             , Map.Map ProcId.ProcId Integer   -- New PC values per visited process.
                             , Integer                         -- Value of the PC at the level of the next behavioral expression.
                             )
constructSeqBExpr procInstUpdateMap createProcInst ownerVidDecls seqPC seqPCValue pcValuePerProc currentBExpr = do
    let defaultConstructBExpr subExprIndex = constructBExpr currentBExpr subExprIndex procInstUpdateMap createProcInst ownerVidDecls seqPC
    let defaultSeqConstructBExpr = constructSeqBExpr procInstUpdateMap createProcInst ownerVidDecls seqPC
    let defaultParConstructBExpr = constructParBExpr procInstUpdateMap createProcInst ownerVidDecls seqPC 0
    let defaultValues pc = let f vid = if vid == seqPC then ValExpr.cstrConst (Constant.Cint pc) else ValExpr.cstrVar vid in
                             map f ownerVidDecls
    let g' = ValExpr.cstrEqual (ValExpr.cstrVar seqPC) (ValExpr.cstrConst (Constant.Cint seqPCValue))
    
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid _cids vexprs) -> do
          r <- getProcById pid
          case r of
            Just (ProcDef.ProcDef _cidDecls vidDecls body) ->
                case pcValuePerProc Map.!? pid of
                  Just initSeqPC -> do
                      let f vid = case List.elemIndex vid vidDecls of
                                    Just i -> vexprs !! i
                                    Nothing -> if vid == seqPC
                                               then ValExpr.cstrConst (Constant.Cint initSeqPC)
                                               else ValExpr.cstrVar vid
                      return (createProcInst (map f ownerVidDecls), Set.empty, pcValuePerProc, seqPCValue)
                  Nothing -> do
                      (_inst, body', pcValuePerProc', seqPCValue') <- defaultSeqConstructBExpr seqPCValue (Map.insert pid seqPCValue pcValuePerProc) body
                      let f vid = case List.elemIndex vid vidDecls of
                                    Just i -> vexprs !! i
                                    Nothing -> if vid == seqPC
                                               then ValExpr.cstrConst (Constant.Cint seqPCValue)
                                               else ValExpr.cstrVar vid
                      return (createProcInst (map f ownerVidDecls), body', pcValuePerProc', seqPCValue')
            Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
      (TxsDefs.view -> Choice bexprs) -> do
          let f (insts, bd, pcs, npc) b = do (inst', bd', pcs', npc') <- defaultSeqConstructBExpr npc pcs b
                                             return (Set.insert inst' insts, Set.union bd bd', pcs', npc')
          (insts, body', pcValuePerProc', seqPCValue') <- Monad.foldM f (Set.empty, Set.empty, pcValuePerProc, seqPCValue + 1) (Set.toList bexprs)
          -- (If there are no choices (e.g. STOP), there will be no summands with PC==seqPCValue, because seqPCValue'>=seqPCValue + 1.)
          return (createProcInst (defaultValues seqPCValue), Set.union (Set.map (guard g') insts) body', pcValuePerProc', seqPCValue')
      (TxsDefs.view -> Parallel cidSet bexprs) -> do
          let f (insts, bd, pcs) b = do (inst', bd', pcs', _) <- defaultParConstructBExpr pcs b
                                        return (Set.insert inst' insts, Set.union bd bd', pcs')
          (insts, body', pcValuePerProc') <- Monad.foldM f (Set.empty, Set.empty, pcValuePerProc) bexprs
          let bexpr' = guard g' (parallel cidSet (Set.toList insts))
          -- Parallel is the end of a branch; it does not increase the PC any further.
          -- It must also communicate the initial value of the PC of the next branch (which must be at least seqPCValue + 1).
          return (createProcInst (defaultValues seqPCValue), Set.insert bexpr' body', pcValuePerProc', seqPCValue + 1)
      (TxsDefs.view -> Guard g bexpr) -> do
          (inst, body', pcValuePerProc', seqPCValue') <- defaultConstructBExpr 0 (seqPCValue + 1) pcValuePerProc bexpr
          let bexpr' = guard (ValExpr.cstrAnd (Set.fromList [g, g'])) inst
          return (createProcInst (defaultValues seqPCValue), Set.insert bexpr' body', pcValuePerProc', seqPCValue')
      (TxsDefs.view -> ActionPref actOffer bexpr) -> do
          (inst, body', pcValuePerProc', seqPCValue') <- defaultConstructBExpr 0 (seqPCValue + 1) pcValuePerProc bexpr
          let actOffer' = addActOfferConjunct actOffer g'
          
          -- Communication variables and state variables (=parameters) should not overlap; this messes up later substitutions.
          -- We ignored this until here, where we substitute the original communication variables with fresh ones:
          let (vizVars, hidVars) = getActOfferVars actOffer
          actOfferSubst <- createFreshVars (Set.union vizVars hidVars)
          let inst' = Subst.subst (Map.map ValExpr.cstrVar actOfferSubst) Map.empty inst
          -- Use a dedicated function to replace variables in ActOffers (because Subst.subst replaces free variables only):
          let bexpr' = actionPref (replaceVarsInActOffer actOfferSubst actOffer') inst'
          
          return (createProcInst (defaultValues seqPCValue), Set.insert bexpr' body', pcValuePerProc', seqPCValue')
      (TxsDefs.view -> Hide cidSet bexpr) -> do
          (inst, body', pcValuePerProc', seqPCValue') <- defaultConstructBExpr 0 seqPCValue pcValuePerProc bexpr
          return (inst, Set.map (applyHide cidSet) body', pcValuePerProc', seqPCValue')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) -> do
          (inst1, inst2, body', pcValuePerProc', seqPCValue') <- getBinDefaultConstructBExpr bexpr1 bexpr2
          let bexpr' = guard g' (enable inst1 acceptOffers inst2)
          return (createProcInst (defaultValues seqPCValue), Set.insert bexpr' body', pcValuePerProc', seqPCValue')
      (TxsDefs.view -> Disable bexpr1 bexpr2) -> do
          (inst1, inst2, body', pcValuePerProc', seqPCValue') <- getBinDefaultConstructBExpr bexpr1 bexpr2
          let bexpr' = guard g' (disable inst1 inst2)
          return (createProcInst (defaultValues seqPCValue), Set.insert bexpr' body', pcValuePerProc', seqPCValue')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) -> do
          (inst1, inst2, body', pcValuePerProc', seqPCValue') <- getBinDefaultConstructBExpr bexpr1 bexpr2
          let bexpr' = guard g' (interrupt inst1 inst2)
          return (createProcInst (defaultValues seqPCValue), Set.insert bexpr' body', pcValuePerProc', seqPCValue')
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
  where
    getBinDefaultConstructBExpr :: TxsDefs.BExpr -> TxsDefs.BExpr -> IOC.IOC ( TxsDefs.BExpr                   -- Instantiation 1.
                                                                             , TxsDefs.BExpr                   -- Instantiation 2.
                                                                             , Set.Set TxsDefs.BExpr           -- Constructed body.
                                                                             , Map.Map ProcId.ProcId Integer   -- New PC values per visited process.
                                                                             , Integer                         -- Next PC value.
                                                                             )
    getBinDefaultConstructBExpr bexpr1 bexpr2 = do
        (inst1, body1, pcs1, seqPCValue1) <- constructBExpr currentBExpr 0 procInstUpdateMap createProcInst ownerVidDecls seqPC (seqPCValue + 1) pcValuePerProc bexpr1
        (inst2, body2, pcs2, seqPCValue2) <- constructBExpr currentBExpr 1 procInstUpdateMap createProcInst ownerVidDecls seqPC (seqPCValue1 + 1) pcs1 bexpr2
        return (inst1, inst2, Set.union body1 body2, pcs2, seqPCValue2)
    -- getBinDefaultConstructBExpr
-- constructBExpr

constructParBExpr :: ProcInstUpdates.ProcInstUpdateMap         -- Contains information about how parallel processes on which we are dependent should be instantiated.
                  -> ([TxsDefs.VExpr] -> TxsDefs.BExpr)        -- Function that constructs a recursive process instantiation.
                  -> [VarId.VarId]                             -- All parameters of the process to which we are adding a sequential PC (including the PC).
                  -> VarId.VarId                               -- The sequential PC.
                  -> Integer                                   -- Value of the PC at the level of the current behavioral expression.
                  -> Map.Map ProcId.ProcId Integer             -- Current PC values per visited process.
                  -> TxsDefs.BExpr                             -- Current behavioral expression.
                  -> IOC.IOC ( TxsDefs.BExpr                   -- Instantiation that can replace the current behavioral expression.
                             , Set.Set TxsDefs.BExpr           -- Body constructed from the current behavioral expression.
                             , Map.Map ProcId.ProcId Integer   -- New PC values per visited process.
                             , Integer                         -- Value of the PC at the level of the next behavioral expression.
                             )
constructParBExpr procInstUpdateMap _createProcInst _ownerVidDecls _seqPC seqPCValue pcValuePerProc currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst {}) -> return (ProcInstUpdates.applyMapToProcInst procInstUpdateMap currentBExpr, Set.empty, pcValuePerProc, seqPCValue + 1)
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- constructParBExpr

constructBExpr :: TxsDefs.BExpr
               -> Int
               -> ProcInstUpdates.ProcInstUpdateMap         -- Contains information about how parallel processes on which we are dependent should be instantiated.
               -> ([TxsDefs.VExpr] -> TxsDefs.BExpr)        -- Function that constructs a recursive process instantiation.
               -> [VarId.VarId]                             -- All parameters of the process to which we are adding a sequential PC (including the PC).
               -> VarId.VarId                               -- The sequential PC.
               -> Integer                                   -- Value of the PC at the level of the current behavioral expression.
               -> Map.Map ProcId.ProcId Integer             -- Current PC values per visited process.
               -> TxsDefs.BExpr                             -- Current behavioral expression.
               -> IOC.IOC ( TxsDefs.BExpr                   -- Instantiation that can replace the current behavioral expression.
                          , Set.Set TxsDefs.BExpr           -- Body constructed from the current behavioral expression.
                          , Map.Map ProcId.ProcId Integer   -- New PC values per visited process.
                          , Integer                         -- Value of the PC at the level of the next behavioral expression.
                          )
constructBExpr currentBExpr subExprIndex =
    case getSubExprType currentBExpr subExprIndex of
      StpSequential -> constructSeqBExpr
      StpPrefixed -> constructSeqBExpr
      StpStackable -> constructParBExpr
      StpUnsafe -> constructParBExpr
-- constructBExpr
















