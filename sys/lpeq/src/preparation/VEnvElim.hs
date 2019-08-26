{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  VEnvElim
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module VEnvElim (
eliminateVEnvs
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified ProcId
import qualified ProcDef
import qualified VarEnv
import qualified Subst
import BehExprDefs
import ProcIdFactory

import ProcSearch

-- Recursively eliminates variable environments from the given behavioral expression.
eliminateVEnvs :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
eliminateVEnvs bexpr = do
    procIds <- getProcsInBExpr bexpr
    Monad.mapM_ doProc (Set.toList procIds)
    elimVEnvs Map.empty bexpr
  where
    doProc :: ProcId.ProcId -> IOC.IOC ()
    doProc pid = do
        r <- getProcById pid
        case r of
          Just (ProcDef.ProcDef cids vids body) -> do
              body' <- elimVEnvs Map.empty body
              registerProc pid (ProcDef.ProcDef cids vids body')
          Nothing -> return ()
-- eliminateVEnvs

elimVEnvs :: VarEnv.VEnv -> TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
elimVEnvs currentVEnv currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid cids vexprs) ->
          return (procInst pid cids (map (Subst.subst currentVEnv Map.empty) vexprs))
      (TxsDefs.view -> Guard g bexpr) ->
          do bexpr' <- elimVEnvs currentVEnv bexpr
             return (guard (Subst.subst currentVEnv Map.empty g) bexpr')
      (TxsDefs.view -> Choice bexprs) ->
          do bexprs' <- Set.fromList <$> Monad.mapM (elimVEnvs currentVEnv) (Set.toList bexprs)
             return (choice bexprs')
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          do bexprs' <- Monad.mapM (elimVEnvs currentVEnv) bexprs
             return (parallel cidSet bexprs')
      (TxsDefs.view -> Hide cidSet bexpr) ->
          do bexpr' <- elimVEnvs currentVEnv bexpr
             return (hide cidSet bexpr')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          do bexpr1' <- elimVEnvs currentVEnv bexpr1
             bexpr2' <- elimVEnvs currentVEnv bexpr2
             return (enable bexpr1' acceptOffers bexpr2')
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do bexpr1' <- elimVEnvs currentVEnv bexpr1
             bexpr2' <- elimVEnvs currentVEnv bexpr2
             return (disable bexpr1' bexpr2')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do bexpr1' <- elimVEnvs currentVEnv bexpr1
             bexpr2' <- elimVEnvs currentVEnv bexpr2
             return (interrupt bexpr1' bexpr2')
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          do bexpr' <- elimVEnvs currentVEnv bexpr
             return (actionPref (doActOffer currentVEnv actOffer) bexpr')
      (TxsDefs.view -> ValueEnv venv bexpr) ->
          do let venv' = Map.map (Subst.subst currentVEnv Map.empty) venv
             -- Note that Map.union is left-biased!
             -- (This means that same-name variables are replaced correctly, assuming that this is even allowed.)
             elimVEnvs (Map.union venv' currentVEnv) bexpr
      -- (TxsDefs.view -> StAut _sid _venv transitions) -> 
          -- ...
      _ -> error ("Behavioral expression not accounted for (\"" ++ show currentBExpr ++ "\")!")
-- elimVEnvs

doActOffer :: VarEnv.VEnv -> TxsDefs.ActOffer -> TxsDefs.ActOffer
doActOffer currentVEnv actOffer = actOffer { TxsDefs.constraint = Subst.subst currentVEnv Map.empty (TxsDefs.constraint actOffer) }

