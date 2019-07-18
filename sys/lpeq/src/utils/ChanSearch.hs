{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ChanSearch
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ChanSearch (
getChansInBExpr
) where

import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ProcId
import qualified ProcDef
import qualified ChanId
import BehExprDefs
import ProcIdFactory

-- Lists all channels that can be reached from the given behavioral expression.
-- Works by depth-first-search.
getChansInBExpr :: [Set.Set ChanId.ChanId] -> [Set.Set ChanId.ChanId] -> TxsDefs.BExpr -> IOC.IOC [ChanId.ChanId]
getChansInBExpr insyncs outsyncs bexpr = Set.toList <$> searchBExprForChans Set.empty (Set.union (Set.unions insyncs) (Set.unions outsyncs)) bexpr

searchBExprForChans :: Set.Set ProcId.ProcId -> Set.Set ChanId.ChanId -> TxsDefs.BExpr -> IOC.IOC (Set.Set ChanId.ChanId)
searchBExprForChans visitedProcs soFar currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid cids _vexprs) ->
             if Set.member pid visitedProcs
             then return soFar
             else do r <- getProcById pid
                     case r of
                       Just (ProcDef.ProcDef cidDecls _vidDecls body) -> do
                           let newCids = Set.fromList (cids ++ cidDecls)
                           searchBExprForChans (Set.insert pid visitedProcs) (Set.union newCids soFar) body
                       Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
      (TxsDefs.view -> Guard _g bexpr) ->
          searchBExprForChans visitedProcs soFar bexpr
      (TxsDefs.view -> Choice bexprs) ->
          Monad.foldM (searchBExprForChans visitedProcs) soFar (Set.toList bexprs)
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          Monad.foldM (searchBExprForChans visitedProcs) (Set.union cidSet soFar) bexprs
      (TxsDefs.view -> Hide cidSet bexpr) ->
          searchBExprForChans visitedProcs (Set.union cidSet soFar) bexpr
      (TxsDefs.view -> Enable bexpr1 _acceptOffers bexpr2) ->
          do soFar' <- searchBExprForChans visitedProcs soFar bexpr1
             searchBExprForChans visitedProcs soFar' bexpr2
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do soFar' <- searchBExprForChans visitedProcs soFar bexpr1
             searchBExprForChans visitedProcs soFar' bexpr2
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do soFar' <- searchBExprForChans visitedProcs soFar bexpr1
             searchBExprForChans visitedProcs soFar' bexpr2
      (TxsDefs.view -> ActionPref _actOffer bexpr) ->
          searchBExprForChans visitedProcs soFar bexpr
      (TxsDefs.view -> ValueEnv _venv bexpr) ->
          searchBExprForChans visitedProcs soFar bexpr
      _ -> error ("Behavioral expression not accounted for (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- searchBExprForChans

