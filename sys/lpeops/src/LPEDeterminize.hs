{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEDeterminize
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEDeterminize (
determinizeLPE
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified EnvData
import qualified FreeVar
import qualified TxsDefs
import qualified ChanId
import qualified VarId
import qualified ValExpr
import qualified Constant
import LPETypes
import LPEBlindSubst
import LPESuccessors
import LPEDeterminism
import UntilFixedPoint
import VarFactory
-- import LPEValidity

-- Makes the LPE deterministic by delaying non-deterministic choices by one step until a fixpoint is reached.
--
-- ******** IMPORTANT ********
--        Contains bugs
-- ***************************
determinizeLPE :: LPEOperation
determinizeLPE lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<det>>" ]
    newLpe <- untilCounterOrFixedPointM 1 (doDetIteration invariant) lpe
    return (Right newLpe)
-- determinizeLPE

doDetIteration :: TxsDefs.VExpr -> LPE -> IOC.IOC LPE
doDetIteration invariant lpe = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Looking for determinizable summand pair in " ++ show (Set.size (lpeSummands lpe)) ++ " summands...") ]
    -- Find a pair of determinizable (and non-deterministic) summands:
    nothingOrDeterminizablePair <- getNonDeterministicSummandPair invariant (Set.toList (lpeSummands lpe))
    case nothingOrDeterminizablePair of
      Just (summand1, summand2) -> do
        let sortedChans1 = List.sortOn (ChanId.unid . fst) (Map.toList (lpeSmdOffers summand1))
        let sortedChans2 = List.sortOn (ChanId.unid . fst) (Map.toList (lpeSmdOffers summand2))
        let chanVars1 = concatMap snd sortedChans1
        let chanVars2 = concatMap snd sortedChans2
        
        doubleVarMappings <- Monad.mapM createDoubleVarMapping (zip chanVars1 chanVars2)
        let chanVar3PerChanVar1 = Map.fromList (map fst doubleVarMappings)
        let chanVar3PerChanVar2 = Map.fromList (map snd doubleVarMappings)
        let chanVars3 = Map.elems chanVar3PerChanVar1
        
        doubleParMappings <- Monad.mapM createDoubleParMapping chanVars3
        let paramPerChanVar3 = Map.fromList (map fst doubleParMappings)
        let chanVar3PerParam = Map.fromList (map snd doubleParMappings)
        let params = Map.keysSet chanVar3PerParam
        
        nonDetFlagVar <- createFreshBoolVarFromPrefix "__NDF"
        
        -- All summands other than summand1 and summand2 are disabled after summand3, because
        -- they do not compensate for the fact that summand3 did not do a state update according to summand1 or summand2.
        -- First initialize some preparatory variables:
        let disableGuard = ValExpr.cstrEqual (ValExpr.cstrVar nonDetFlagVar) (ValExpr.cstrConst (Constant.Cbool False))
        let extraSmdEqs = Map.insert nonDetFlagVar (ValExpr.cstrConst (Constant.Cbool False)) (selfLoopParamEqs params)
        let disableSmd = getDisabledSummand disableGuard extraSmdEqs
        
        -- Build 'the disabled summands' here:
        let disabledSummands = map disableSmd (Set.toList (lpeSummands lpe Set.\\ Set.fromList [summand1, summand2]))
        
        -- Build summand 1.
        -- Summand 1 requires that summand1 is enabled and that summand2 is disabled.
        -- Summand 1 is also disabled immediately after summand 3.
        --
        -- When enabled, summand 1 follows the original behavior of summand1.
        -- (No fresh communication variables are created because summand1 is not part of the new LPE.)
        let useChanVars1 = Map.fromList (zipWith (\cv1 cv2 -> (cv2, ValExpr.cstrVar cv1)) chanVars1 chanVars2)
        guard2' <- doConfidentSubst summand2 useChanVars1 (lpeSmdGuard summand2)
        let newSummand1 = disableSmd (summand1 { lpeSmdGuard = ValExpr.cstrAnd (Set.fromList [lpeSmdGuard summand1, ValExpr.cstrNot guard2']) })
        
        -- Build summand 2.
        -- Summand 2 requires that summand2 is enabled and that summand1 is disabled.
        -- Summand 2 is also disabled immediately after summand 3.
        --
        -- When enabled, summand 2 follows the original behavior of summand2.
        -- (No fresh communication variables are created because summand2 is not part of the new LPE.)
        let useChanVars2 = Map.fromList (zipWith (\cv2 cv1 -> (cv1, ValExpr.cstrVar cv2)) chanVars2 chanVars1)
        guard1' <- doConfidentSubst summand1 useChanVars2 (lpeSmdGuard summand1)
        let newSummand2 = disableSmd (summand2 { lpeSmdGuard = ValExpr.cstrAnd (Set.fromList [lpeSmdGuard summand2, ValExpr.cstrNot guard1']) })
        
        -- Build summand 3.
        -- Summand 3 requires that summand1 and summand2 are enabled simultaneously.
        --
        -- It also captures any communicated values in freshly created communication variables.
        -- The communicated values are subsequently stored in freshly created parameters, and
        -- the values of the original parameters are not changed;
        -- this makes it possible to delay the decision between summand1 and summand2.
        --
        -- Finally, summand 3 sets a freshly created flag variable to true.
        -- This flag enables the modified potential-successor summands that we create below, and
        -- disables all other summands (as described above).
        guard1'' <- doConfidentSubst summand1 (Map.map ValExpr.cstrVar chanVar3PerChanVar1) (lpeSmdGuard summand1)
        guard2'' <- doConfidentSubst summand2 (Map.map ValExpr.cstrVar chanVar3PerChanVar2) (lpeSmdGuard summand2)
        let guard3 = ValExpr.cstrAnd (Set.fromList [disableGuard, guard1'', guard2''])
        let vars3 = Set.union (Set.fromList chanVars3) (Set.fromList (FreeVar.freeVars guard3))
        let newSummand3 = LPESummand { lpeSmdVars = (vars3 Set.\\ lpeParams lpe) Set.\\ Set.insert nonDetFlagVar params
                                     , lpeSmdOffers = Map.map (map (\v -> chanVar3PerChanVar1 Map.! v)) (lpeSmdOffers summand1)
                                     , lpeSmdGuard = guard3
                                     , lpeSmdEqs =
                                         Map.union
                                           -- Set the non-determinism flag to true, and do not change the original parameters:
                                           (Map.insert nonDetFlagVar (ValExpr.cstrConst (Constant.Cbool True)) (selfLoopParamEqs (lpeParams lpe)))
                                           -- Store the communicated values in the freshly created parameters:
                                           (Map.map ValExpr.cstrVar chanVar3PerParam)
                                     }
        
        -- Summands that potentially succeed summand1 or summand2 are potentially successors of the constructed summand 3 as well.
        -- They must therefore be enabled immediately after summand 3.
        -- They must also apply the parameter assignments of summand1 or summand2 in their guard, since summand 3 did not do this.
        let enableGuard = ValExpr.cstrEqual (ValExpr.cstrVar nonDetFlagVar) (ValExpr.cstrConst (Constant.Cbool True))
        let useParams1 = Map.map (\v -> ValExpr.cstrVar (paramPerChanVar3 Map.! v)) chanVar3PerChanVar1
        let useParams2 = Map.map (\v -> ValExpr.cstrVar (paramPerChanVar3 Map.! v)) chanVar3PerChanVar2
        let getEnabledSmd1 = getEnabledSummand enableGuard summand1 useParams1 extraSmdEqs
        let getEnabledSmd2 = getEnabledSummand enableGuard summand2 useParams2 extraSmdEqs
        
        -- Build the enabled summands (for both summand1 and summand2):
        possibleSuccessors1 <- getPossibleSuccessors (lpeSummands lpe) invariant summand1
        possibleSuccessors2 <- getPossibleSuccessors (lpeSummands lpe) invariant summand2
        enabledSummands1 <- Monad.mapM getEnabledSmd1 possibleSuccessors1
        enabledSummands2 <- Monad.mapM getEnabledSmd2 possibleSuccessors2
        
        let allSummands = map updateSmdVars (disabledSummands ++ [newSummand1, newSummand2, newSummand3] ++ enabledSummands1 ++ enabledSummands2)
        
        -- let summandVal = validateLPESummand "Determinize" (Map.keysSet (lpeSmdEqs newSummand3))
        -- Monad.when (concatMap summandVal disabledSummands /= []) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Problems in disabled summands!") ])
        -- Monad.when (summandVal newSummand1 /= []) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Problems in enabled summand 1!") ])
        -- Monad.when (summandVal newSummand2 /= []) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Problems in enabled summand 2!") ])
        -- Monad.when (summandVal newSummand3 /= []) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Problems in enabled summand 3!") ])
        -- Monad.when (concatMap summandVal enabledSummands1 /= []) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Problems in enabled summands 1!") ])
        -- Monad.when (concatMap summandVal enabledSummands2 /= []) (IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Problems in enabled summands 2!") ])
        
        -- Combine everything:
        let newLpe = lpe { lpeInitEqs = Map.union
                                          -- Set the non-determinism flag to true, and add it to the original parameters:
                                          (Map.insert nonDetFlagVar (ValExpr.cstrConst (Constant.Cbool False)) (lpeInitEqs lpe))
                                          -- Set newly created parameters to a default value:
                                          (defaultValueParamEqs (lpeContext lpe) params)
                         , lpeSummands = Set.fromList allSummands }
        
        let newSummandCount = Set.size (lpeSummands newLpe) - Set.size (lpeSummands lpe)
        IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Added " ++ show newSummandCount ++ " summands to LPE") ]
        
        return newLpe
      Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_ANY "No determinizable summand pair found!" ]
                    return lpe
  where
    createDoubleVarMapping :: (VarId.VarId, VarId.VarId) -> IOC.IOC ((VarId.VarId, VarId.VarId), (VarId.VarId, VarId.VarId))
    createDoubleVarMapping (varId1, varId2) = do
        freshVar <- createFreshVarFromVar varId1
        return ((varId1, freshVar), (varId2, freshVar))
    -- createDoubleVarMapping
    
    createDoubleParMapping :: VarId.VarId -> IOC.IOC ((VarId.VarId, VarId.VarId), (VarId.VarId, VarId.VarId))
    createDoubleParMapping varId = do
        freshPar <- createFreshVarFromVar varId
        return ((varId, freshPar), (freshPar, varId))
    -- createDoubleParMapping
    
    getDisabledSummand :: TxsDefs.VExpr -> LPEParamEqs -> LPESummand -> LPESummand
    getDisabledSummand disableGuard extraSmdEqs summand =
        summand { lpeSmdGuard = ValExpr.cstrAnd (Set.fromList [disableGuard, lpeSmdGuard summand])
                , lpeSmdEqs = Map.union (lpeSmdEqs summand) extraSmdEqs
                }
    -- getDisabledSummand
    
    getEnabledSummand :: TxsDefs.VExpr -> LPESummand -> LPEParamEqs -> LPEParamEqs -> LPESummand -> IOC.IOC LPESummand
    getEnabledSummand enableGuard eqsSummand useParams extraSmdEqs summand = do
        useEqs <- doConfidentParamEqsSubst eqsSummand useParams (lpeSmdEqs eqsSummand)
        g <- doConfidentSubst summand useEqs (lpeSmdGuard summand)
        return (summand { lpeSmdGuard = ValExpr.cstrAnd (Set.fromList [enableGuard, g])
                        , lpeSmdEqs = Map.union (lpeSmdEqs summand) extraSmdEqs
                        })
    -- getEnabledSummand
    
    updateSmdVars :: LPESummand -> LPESummand
    updateSmdVars summand =
        let guardVars = Set.fromList (FreeVar.freeVars (lpeSmdGuard summand)) in
        let eqsVars = Set.fromList (concatMap FreeVar.freeVars (Map.elems (lpeSmdEqs summand))) in
        let params = Map.keysSet (lpeSmdEqs summand) in
          summand { lpeSmdVars = Set.union guardVars eqsVars Set.\\ params }
-- doDetIteration

