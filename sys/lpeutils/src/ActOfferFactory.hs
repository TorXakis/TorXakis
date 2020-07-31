{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ActOfferFactory
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module ActOfferFactory (
emptyActOffer,
doActOfferSubst,
replaceVarsInActOffer,
replaceVarsInOffer,
replaceVarsInChanOffer,
getOfferVarsPerChan,
getActOfferVars,
getActOfferVarSet,
getOfferVars,
getChanOfferVar,
getActOfferChans,
mergeActOffers,
addActOfferConjunct,
removeChanFromActOffer
) where

-- import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified TxsDefs
import qualified TxsShow
import qualified ValExpr
import qualified ChanId
import qualified VarId
import qualified Subst

import ValFactory

emptyActOffer :: TxsDefs.ActOffer
emptyActOffer = TxsDefs.ActOffer { TxsDefs.offers = Set.empty
                                 , TxsDefs.hiddenvars = Set.empty
                                 , TxsDefs.constraint = cstrFalse
                                 }
-- emptyActOffer

doActOfferSubst :: Map.Map VarId.VarId TxsDefs.VExpr -> TxsDefs.ActOffer -> TxsDefs.ActOffer
doActOfferSubst subst actOffer =
    actOffer { TxsDefs.constraint = Subst.subst subst Map.empty (TxsDefs.constraint actOffer) }
-- doActOfferSubst

replaceVarsInActOffer :: Map.Map VarId.VarId VarId.VarId -> TxsDefs.ActOffer -> TxsDefs.ActOffer
replaceVarsInActOffer subst actOffer =
    let valSubst = Subst.subst (Map.map ValExpr.cstrVar subst) Map.empty in
      TxsDefs.ActOffer { TxsDefs.offers = Set.map (replaceVarsInOffer subst) (TxsDefs.offers actOffer)
                       , TxsDefs.hiddenvars = Set.map (replaceVar subst) (TxsDefs.hiddenvars actOffer)
                       , TxsDefs.constraint = valSubst (TxsDefs.constraint actOffer)
                       }
-- replaceVarsInActOffer

replaceVarsInOffer :: Map.Map VarId.VarId VarId.VarId -> TxsDefs.Offer -> TxsDefs.Offer
replaceVarsInOffer subst offer = offer { TxsDefs.chanoffers = map (replaceVarsInChanOffer subst) (TxsDefs.chanoffers offer) }

replaceVarsInChanOffer :: Map.Map VarId.VarId VarId.VarId -> TxsDefs.ChanOffer -> TxsDefs.ChanOffer
replaceVarsInChanOffer subst (TxsDefs.Quest v) = TxsDefs.Quest (replaceVar subst v)
replaceVarsInChanOffer _ chanOffer = error ("ChanOffer should not contain an Exclam (\"" ++ TxsShow.fshow chanOffer ++ "\")!")

replaceVar :: Map.Map VarId.VarId VarId.VarId -> VarId.VarId -> VarId.VarId
replaceVar subst v = Map.findWithDefault v v subst

-- replaceVarWithError :: Map.Map VarId.VarId VarId.VarId -> VarId.VarId -> VarId.VarId
-- replaceVarWithError subst v = Map.findWithDefault (error ("Could not find " ++ TxsShow.fshow v ++ " in {" ++ List.intercalate ", " (map TxsShow.fshow (Map.keys subst)) ++ "}")) v substA

getOfferVarsPerChan :: TxsDefs.ActOffer -> Map.Map ChanId.ChanId [VarId.VarId]
getOfferVarsPerChan actOffer = Map.fromList (map (\o -> (TxsDefs.chanid o, getOfferVars o)) (Set.toList (TxsDefs.offers actOffer)))

getActOfferVars :: TxsDefs.ActOffer -> (Set.Set VarId.VarId, Set.Set VarId.VarId)
getActOfferVars actOffer = (Set.fromList (concatMap getOfferVars (TxsDefs.offers actOffer)), TxsDefs.hiddenvars actOffer)

getActOfferVarSet :: TxsDefs.ActOffer -> Set.Set VarId.VarId
getActOfferVarSet actOffer = let (vizVars, hidVars) = getActOfferVars actOffer in Set.union vizVars hidVars

getOfferVars :: TxsDefs.Offer -> [VarId.VarId]
getOfferVars offer = map getChanOfferVar (TxsDefs.chanoffers offer)

getChanOfferVar :: TxsDefs.ChanOffer -> VarId.VarId
getChanOfferVar (TxsDefs.Quest v) = v
getChanOfferVar chanOffer = error ("ChanOffer should not contain an Exclam (\"" ++ TxsShow.fshow chanOffer ++ "\")!")

getActOfferChans :: TxsDefs.ActOffer -> Set.Set ChanId.ChanId
getActOfferChans actOffer = Set.map TxsDefs.chanid (TxsDefs.offers actOffer)

mergeActOffers :: TxsDefs.ActOffer -> TxsDefs.ActOffer -> TxsDefs.ActOffer
mergeActOffers actOffer1 actOffer2 =
    TxsDefs.ActOffer { TxsDefs.offers = Set.union (TxsDefs.offers actOffer1) (TxsDefs.offers actOffer2)
                     , TxsDefs.hiddenvars = Set.union (TxsDefs.hiddenvars actOffer1) (TxsDefs.hiddenvars actOffer2)
                     , TxsDefs.constraint = ValExpr.cstrAnd (Set.fromList [TxsDefs.constraint actOffer1, TxsDefs.constraint actOffer2])
                     }
-- mergeActOffers

addActOfferConjunct :: TxsDefs.ActOffer -> TxsDefs.VExpr -> TxsDefs.ActOffer
addActOfferConjunct actOffer conjunct = actOffer { TxsDefs.constraint = ValExpr.cstrAnd (Set.fromList [conjunct, TxsDefs.constraint actOffer]) }

removeChanFromActOffer :: TxsDefs.ActOffer -> ChanId.ChanId -> TxsDefs.ActOffer
removeChanFromActOffer actOffer targetChan =
    let (match, mismatch) = Set.partition (\offer -> TxsDefs.chanid offer == targetChan) (TxsDefs.offers actOffer) in
      actOffer { TxsDefs.offers = mismatch
               , TxsDefs.hiddenvars = Set.union (TxsDefs.hiddenvars actOffer) (Set.fromList (concatMap getOfferVars (Set.toList match)))
               }
-- removeChanFromActOffer


