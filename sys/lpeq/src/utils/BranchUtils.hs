{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  BranchUtils
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

-- Branches are often synonymous with summands.
-- A different name is used while a linearized process expression (LPE) is still under construction.
-- Branches may also include a HIDE structure, which was not permitted in earlier versions of LPEs.
module BranchUtils (
removeHide,
applyHide,
applyHideToEither,
applyHideToActOffer,
getBranches,
getBranchSegments,
getBranchChans
) where

import qualified Data.Set as Set
import qualified TxsDefs
import qualified TxsShow
import qualified ChanId
import qualified VarId
import BehExprDefs
import ActOfferFactory
import SetUnions

-- This function does the following:
--  - If the given expression has a HIDE on the outside, removes the HIDE, and
--    returns the inner expression and the channels that were hidden.
--  - If the given expression does not have a HIDE on the outside,
--    returns the same expression.
removeHide :: TxsDefs.BExpr -> (TxsDefs.BExpr, Set.Set ChanId.ChanId)
removeHide (TxsDefs.view -> Hide cidSet bexpr) = (bexpr, cidSet)
removeHide bexpr = (bexpr, Set.empty)

-- Wraps the given expression in a HIDE (only if the given set of channels is non-empty).
applyHide :: Set.Set ChanId.ChanId -> TxsDefs.BExpr -> TxsDefs.BExpr
applyHide hiddenChanSet (TxsDefs.view -> Hide cidSet bexpr) = applyHide (Set.union hiddenChanSet cidSet) bexpr
applyHide hiddenChanSet bexpr =
    if Set.null hiddenChanSet
    then bexpr
    else hide hiddenChanSet bexpr
-- applyHide

-- Applies 'applyHide' to the inner expression of 'Left ...' or 'Right ...'.
applyHideToEither :: Set.Set ChanId.ChanId -> Either TxsDefs.BExpr TxsDefs.BExpr -> Either TxsDefs.BExpr TxsDefs.BExpr
applyHideToEither hiddenChanSet (Left bexpr) = Left (applyHide hiddenChanSet bexpr)
applyHideToEither hiddenChanSet (Right bexpr) = Right (applyHide hiddenChanSet bexpr)

-- Removes the given channels from an ActOffer.
applyHideToActOffer :: Set.Set ChanId.ChanId -> TxsDefs.ActOffer -> TxsDefs.ActOffer
applyHideToActOffer hiddenChanSet actOffer =
    let (newOffers, newHiddenVars) = foldl getNewOffers (Set.empty, TxsDefs.hiddenvars actOffer) (TxsDefs.offers actOffer) in
      actOffer { TxsDefs.offers = newOffers, TxsDefs.hiddenvars = newHiddenVars }
  where
    getNewOffers :: (Set.Set TxsDefs.Offer, Set.Set VarId.VarId) -> TxsDefs.Offer -> (Set.Set TxsDefs.Offer, Set.Set VarId.VarId)
    getNewOffers (offersSoFar, hiddenVarsSoFar) offer =
        if Set.member (TxsDefs.chanid offer) hiddenChanSet
        then (offersSoFar, Set.union hiddenVarsSoFar (Set.fromList (getOfferVars offer)))
        else (Set.insert offer offersSoFar, hiddenVarsSoFar)
    -- getNewOffers
-- applyHideToActOffer

-- Flattens a hierarchy of Choice expressions.
-- (Usually equivalent to providing a list of summands.)
getBranches :: TxsDefs.BExpr -> Set.Set TxsDefs.BExpr
getBranches (TxsDefs.view -> Choice bexprs) = setUnions (Set.map getBranches bexprs)
getBranches bexpr = Set.singleton bexpr

-- Splits an expression (also called a branch or a summand) into 3 parts:
--  - Set of hidden channels.
--  - ActOffer.
--  - Process instantiation.
getBranchSegments :: TxsDefs.BExpr -> (Set.Set ChanId.ChanId, TxsDefs.ActOffer, TxsDefs.BExpr)
getBranchSegments currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> Hide cidSet bexpr) -> getFromInnerExpr cidSet bexpr
      _ -> getFromInnerExpr Set.empty currentBExpr
  where
    getFromInnerExpr :: Set.Set ChanId.ChanId -> TxsDefs.BExpr -> (Set.Set ChanId.ChanId, TxsDefs.ActOffer, TxsDefs.BExpr)
    getFromInnerExpr cidSet innerExpr =
        case innerExpr of
          (TxsDefs.view -> ActionPref actOffer bexpr) ->
              case bexpr of
                (TxsDefs.view -> ProcInst {}) -> (cidSet, applyHideToActOffer cidSet actOffer, bexpr)
                _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
          _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
    -- getFromInnerExpr
-- getBranchChan

-- Extracts the channels from a branch.
getBranchChans :: TxsDefs.BExpr -> Set.Set ChanId.ChanId
getBranchChans currentBExpr = let (_, actOffer, _) = getBranchSegments currentBExpr in getActOfferChans actOffer



