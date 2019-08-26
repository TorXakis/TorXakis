{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HideElim
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module HideElim (
eliminateHide
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ValExpr
import qualified ProcId
import qualified ProcDef
import qualified ChanId
import qualified VarId
import BehExprDefs
import ProcIdFactory
import ActOfferFactory
import VarFactory
import ValFactory

import qualified ProcInstUpdates

import BranchUtils

eliminateHide :: (ProcId.ProcId, ProcInstUpdates.ProcInstUpdateMap) -> IOC.IOC (ProcId.ProcId, ProcInstUpdates.ProcInstUpdateMap)
eliminateHide (pid, procInstUpdateMap) = do
    IOC.putInfo [ "eliminateHideFrom " ++ TxsShow.fshow pid ]
    r <- getProcById pid
    case r of
      Just (ProcDef.ProcDef cidDecls vidDecls body) -> do
          let branches = Set.toList (getBranches body)
          let hiddenChans = Set.toList (Set.unions (map (snd . removeHide) branches))
          IOC.putInfo [ "hiddenChans = " ++ show hiddenChans ]
          if List.null hiddenChans
          then return (pid, procInstUpdateMap)
          else do flags <- Monad.mapM (createFreshBoolVarFromPrefix . (\cid -> "flag" ++ Text.unpack (ChanId.name cid))) hiddenChans
                  let flagPerChan = Map.fromList (zip hiddenChans flags)
                  
                  newProcInstUpdate <- ProcInstUpdates.createWithFreshPid pid vidDecls (vidDecls ++ flags) (Map.fromList (map (, cstrTrue) flags))
                  let procInstUpdateMap' = ProcInstUpdates.addToMap procInstUpdateMap pid newProcInstUpdate
                  
                  newBranches <- Set.unions <$> Monad.mapM (createFlaggedBranches (fst newProcInstUpdate) hiddenChans flagPerChan) branches
                  registerProc (fst newProcInstUpdate) (ProcDef.ProcDef cidDecls (vidDecls ++ flags) (choice newBranches))
                  
                  return (fst newProcInstUpdate, procInstUpdateMap')
      Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
-- eliminateHide

createFlaggedBranches :: ProcId.ProcId -> [ChanId.ChanId] -> Map.Map ChanId.ChanId VarId.VarId -> TxsDefs.BExpr -> IOC.IOC (Set.Set TxsDefs.BExpr)
createFlaggedBranches ownerPid hiddenChans flagPerChan bexpr =
    Set.fromList <$> Monad.mapM (getGuardedBranch . Set.fromList) (List.subsequences hiddenChans)
  where
    getGuardedBranch :: Set.Set ChanId.ChanId -> IOC.IOC TxsDefs.BExpr
    getGuardedBranch chansEnabledByGuard = do
        let (chansDisabledByThisBranch, actOffer, recProcInst) = getBranchSegments bexpr
        
        let chansDisabledByGuard = Set.fromList hiddenChans Set.\\ chansEnabledByGuard
        let chansDisabledAfterThisBranch = Set.union chansDisabledByGuard chansDisabledByThisBranch
        
        let f1 cid = let vid = ValExpr.cstrVar (flagPerChan Map.! cid) in
                       if Set.member cid chansEnabledByGuard then vid else ValExpr.cstrNot vid
        let guardedActOffer = addActOfferConjunct actOffer (ValExpr.cstrAnd (Set.fromList (map f1 hiddenChans)))
        
        let f2 cid = if Set.member cid chansDisabledAfterThisBranch then cstrFalse else cstrTrue
        let flagVExprs = map f2 hiddenChans
        
        case recProcInst of
          (TxsDefs.view -> ProcInst _pid cids vexprs) -> do
              let newActOffer = applyHideToActOffer chansDisabledAfterThisBranch guardedActOffer
              let newProcInst = procInst ownerPid cids (vexprs ++ flagVExprs)
              return (actionPref newActOffer newProcInst)
          _ -> error ("ProcInst expected but found " ++ TxsShow.fshow recProcInst)
    -- getGuardedBranch
-- createFlaggedBranches

























