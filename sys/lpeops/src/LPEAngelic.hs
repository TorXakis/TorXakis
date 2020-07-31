{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEAngelic
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEAngelic (
makeInputEnabledLPE
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified EnvData
import qualified TxsDefs
import qualified ChanId
import qualified FreeVar
import qualified VarId
import qualified ValExpr
import LPETypes
import LPEBlindSubst
import VarFactory

-- Makes the LPE deterministic by delaying non-deterministic choices by one step until a fixpoint is reached.
makeInputEnabledLPE :: LPEOperation
makeInputEnabledLPE lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<angelic>>" ]
    let summandsByInput = partitionMap (Map.keysSet . lpeSmdOffers) (Set.toList (getInputSummands lpe))
    inputEnablingSummands <- Monad.mapM (makeInputEnablingSummand lpe) (Map.toList summandsByInput)
    return (Right (lpe { lpeSummands = Set.union (lpeSummands lpe) (Set.fromList inputEnablingSummands) }))
-- makeInputEnabledLPE

-- Returns all summands of an LPE that use an input channel.
getInputSummands :: LPE -> LPESummands
getInputSummands lpe = Set.filter (usesInChan (lpeInChans lpe)) (lpeSummands lpe)
  where
    usesInChan :: Set.Set (Set.Set ChanId.ChanId) -> LPESummand -> Bool
    usesInChan inChans summand = Set.member (Map.keysSet (lpeSmdOffers summand)) inChans
-- getInputSummands

-- Partitions a list based on a key provided by a function.
partitionMap :: Ord a => (b -> a) -> [b] -> Map.Map a [b]
partitionMap f xs = foldl (Map.unionWith (++)) Map.empty (map (\x -> Map.singleton (f x) [x]) xs)

-- Creates a new summand that, when added to the original LPE, makes that LPE input-enabled with regard to the input channel of the summand.
-- Parameter summands must contain all summands of the original LPE that use a specific input channel.
makeInputEnablingSummand :: LPE -> (Set.Set ChanId.ChanId, [LPESummand]) -> IOC.IOC LPESummand
makeInputEnablingSummand lpe (inChan, summands) = do
    sortedSmdOffers <- List.sortOn (ChanId.unid . fst) <$> Monad.mapM chanToOffer (Set.toList inChan)
    let sortedSmdVars = concatMap snd sortedSmdOffers
    guards <- Monad.mapM (summandToGuard sortedSmdVars) summands
    let smdGuard = ValExpr.cstrAnd (Set.fromList (map ValExpr.cstrNot guards))
    return (LPESummand { -- List all variables that occur in the guard, except LPE parameters:
                         lpeSmdVars = Set.fromList (FreeVar.freeVars smdGuard) Set.\\ lpeParams lpe
                       , lpeSmdOffers = Map.fromList sortedSmdOffers
                         -- The new guard ensures that the new summand is enabled iff none of the old summands is enabled:
                       , lpeSmdGuard = smdGuard
                         -- The new summand does a self-loop:
                       , lpeSmdEqs = selfLoopParamEqs (lpeParams lpe)
                       })
  where
    -- Creates fresh channel variables based on a channel signature.
    chanToOffer :: ChanId.ChanId -> IOC.IOC LPEChanOffer
    chanToOffer chanId = do
        freshVars <- Monad.mapM createFreshVar (ChanId.chansorts chanId)
        return (chanId, freshVars)
    -- chanToOffer
    
    -- Maps the variables in the guard of an old summand to the channel variables of the new summand:
    summandToGuard :: [VarId.VarId] -> LPESummand -> IOC.IOC TxsDefs.VExpr
    summandToGuard sortedSmdVars summand = do
        let sortedOffers = List.sortOn (ChanId.unid . fst) (Map.toList (lpeSmdOffers summand))
        let subst = Map.fromList (zipWith (\cv1 cv2 -> (cv2, ValExpr.cstrVar cv1)) sortedSmdVars (concatMap snd sortedOffers))
        doConfidentSubst summand subst (lpeSmdGuard summand)
-- makeInputEnablingSummand

