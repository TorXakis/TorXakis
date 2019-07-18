{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ExclamToQuest
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ExclamToQuest (
exclamToQuest
) where

import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified ValExpr
import qualified ProcId
import qualified SortOf
import qualified ProcDef
import BehExprDefs
import ProcIdFactory
import VarFactory

import ProcSearch

-- Recursively replaces occurrences of !x with ?x.
exclamToQuest :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
exclamToQuest bexpr = do
    procIds <- getProcsInBExpr bexpr
    Monad.mapM_ doProc (Set.toList procIds)
    exclamToQst bexpr
  where
    doProc :: ProcId.ProcId -> IOC.IOC ()
    doProc pid = do
        r <- getProcById pid
        case r of
          Just (ProcDef.ProcDef cids vids body) -> do
              body' <- exclamToQst body
              registerProc pid (ProcDef.ProcDef cids vids body')
          Nothing -> error ("Unknown process (\"" ++ show pid ++ "\")!")
-- exclamToQuest

exclamToQst :: TxsDefs.BExpr -> IOC.IOC TxsDefs.BExpr
exclamToQst currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst _pid _cids _vexprs) ->
          return currentBExpr
      (TxsDefs.view -> Guard g bexpr) ->
          do bexpr' <- exclamToQst bexpr
             return (guard g bexpr')
      (TxsDefs.view -> Choice bexprs) ->
          do bexprs' <- Set.fromList <$> Monad.mapM exclamToQst (Set.toList bexprs)
             return (choice bexprs')
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          do bexprs' <- Monad.mapM exclamToQst bexprs
             return (parallel cidSet bexprs')
      (TxsDefs.view -> Hide cidSet bexpr) ->
          do bexpr' <- exclamToQst bexpr
             return (hide cidSet bexpr')
      (TxsDefs.view -> Enable bexpr1 acceptOffers bexpr2) ->
          do bexpr1' <- exclamToQst bexpr1
             (acceptOffers', extraConditions) <- doChanOffers acceptOffers
             bexpr2' <- exclamToQst bexpr2
             return (enable bexpr1' acceptOffers' (guard (ValExpr.cstrAnd (Set.fromList extraConditions)) bexpr2'))
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do bexpr1' <- exclamToQst bexpr1
             bexpr2' <- exclamToQst bexpr2
             return (disable bexpr1' bexpr2')
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do bexpr1' <- exclamToQst bexpr1
             bexpr2' <- exclamToQst bexpr2
             return (interrupt bexpr1' bexpr2')
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          do actOffer' <- doActOffer actOffer
             bexpr' <- exclamToQst bexpr
             return (actionPref actOffer' bexpr')
      (TxsDefs.view -> ValueEnv venv bexpr) ->
          do bexpr' <- exclamToQst bexpr
             return (valueEnv venv bexpr')
      -- (TxsDefs.view -> StAut _sid _venv transitions) -> 
          -- ...
      _ -> error ("Behavioral expression not accounted for (\"" ++ show currentBExpr ++ "\")!")
-- exclamToQst

doActOffer :: TxsDefs.ActOffer -> IOC.IOC TxsDefs.ActOffer
doActOffer actOffer = do
    r <- Monad.mapM doOffer (Set.toList (TxsDefs.offers actOffer))
    let newOffers = Set.fromList (map fst r)
    let newConstraint = ValExpr.cstrAnd (Set.fromList (TxsDefs.constraint actOffer : concatMap snd r))
    return (actOffer { TxsDefs.offers = newOffers, TxsDefs.constraint = newConstraint })
-- doActOffer

doOffer :: TxsDefs.Offer -> IOC.IOC (TxsDefs.Offer, [TxsDefs.VExpr])
doOffer offer = do
    (newChanOffers, extraConditions) <- doChanOffers (TxsDefs.chanoffers offer)
    return (offer { TxsDefs.chanoffers = newChanOffers }, extraConditions)
-- doOffer

doChanOffers :: [TxsDefs.ChanOffer] -> IOC.IOC ([TxsDefs.ChanOffer], [TxsDefs.VExpr])
doChanOffers offerList = do
    r <- Monad.mapM doChanOffer offerList
    return (map fst r, concatMap snd r)
-- doChanOffer

doChanOffer :: TxsDefs.ChanOffer -> IOC.IOC (TxsDefs.ChanOffer, [TxsDefs.VExpr])
doChanOffer (TxsDefs.Quest vid) = return (TxsDefs.Quest vid, [])
doChanOffer (TxsDefs.Exclam vexpr) = do
    newVid <- createFreshVar (SortOf.sortOf vexpr)
    return (TxsDefs.Quest newVid, [ValExpr.cstrEqual (ValExpr.cstrVar newVid) vexpr])
-- doChanOffer


