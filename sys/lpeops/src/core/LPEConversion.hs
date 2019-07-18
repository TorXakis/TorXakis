{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEConversion
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

-- {-# LANGUAGE ViewPatterns        #-}
module LPEConversion (
model2lpe,
lpe2model,
module LPETypes
) where

--import qualified Control.Monad as Monad
import qualified Control.Monad as Monad
import qualified Control.Monad.State as MonadState
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified EnvCore as IOC
import qualified EnvData
import qualified SortOf
import qualified TxsDefs
import qualified TxsShow
import qualified ProcId
import qualified VarId
import qualified ValExpr
import qualified ModelId
import qualified ChanId
import           LPEValidity
import           LPETypes
import           ConcatEither
import           BlindSubst
import           ModelIdFactory
import           LPEChanMap
import           VarFactory
import           ChanFactory
import           ChanAlphabet
import           LPEBlindSubst

-- Constructs an LPEModel from a process expression (unless there are problems).
-- The process expression should be the instantiation of a process that has already been linearized!
model2lpe :: TxsDefs.ModelId -> IOC.IOC (Either [String] LPE)
model2lpe modelId = do
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    let mdefs = TxsDefs.modelDefs tdefs
    case [ mdef | (mid, mdef) <- Map.toList mdefs, mid == modelId ] of
      (TxsDefs.ModelDef inChans outChans splsyncs procInst:_) ->
        case TxsDefs.view procInst of
          TxsDefs.ProcInst procId _ paramValues -> do
            let pdefs = TxsDefs.procDefs tdefs
            case pdefs Map.!? procId of
              Just procDef@(TxsDefs.ProcDef _chans params body) ->
                case getParamEqs tdefs "model initiation" params paramValues of
                  Left msgs -> return (Left msgs)
                  Right initEqs -> case getValidatedSummandData tdefs procId procDef body of
                                     Left msgs -> return (Left msgs)
                                     Right summandData -> do let lpe = emptyLPE { lpeContext = tdefs
                                                                                , lpeSplSyncs = splsyncs -- (We do not really know what this is for, we just remember its value!)
                                                                                , lpeName = Text.pack (Text.unpack (ModelId.name modelId) ++ "_" ++ Text.unpack (ProcId.name procId))
                                                                                , lpeInitEqs = initEqs
                                                                                }
                                                             let chanAlphabet = getChanAlphabet inChans outChans
                                                             IOC.putMsgs [ EnvData.TXS_CORE_ANY ("INS = {" ++ concatMap bla inChans ++ "}") ]
                                                             IOC.putMsgs [ EnvData.TXS_CORE_ANY ("OUTS = {" ++ concatMap bla outChans ++ "}") ]
                                                             IOC.putMsgs [ EnvData.TXS_CORE_ANY ("ALPHABET = {" ++ concatMap bla (Set.toList chanAlphabet) ++ "}") ]
                                                             summandData' <- ensureFreshCommVars (filterSummandDataByChanAlphabet chanAlphabet summandData)
                                                             (refinedSummandData, chanMap) <- refineSummandData summandData'
                                                             let lpe' = lpe { lpeChanMap = chanMap
                                                                            , lpeInChans = Map.keysSet (permittedChanMap (lpeChanMap lpe') inChans)
                                                                            , lpeOutChans = Map.keysSet (permittedChanMap (lpeChanMap lpe') outChans)
                                                                            , lpeSummands = Set.fromList (map (constructFromRefinedSummandData chanMap) refinedSummandData)
                                                                            }
                                                             let problems = validateLPEModel lpe'
                                                             if null problems
                                                             then return (Right lpe')
                                                             else return (Left problems)
              Nothing -> do let definedProcessNames = List.intercalate " or " (map (Text.unpack . ProcId.name) (Map.keys pdefs))
                            return (Left ["Expected " ++ definedProcessNames ++ ", found " ++ show (Text.unpack (ProcId.name procId)) ++ "!"])
          _ -> return (Left ["Expected process instantiation, found " ++ TxsShow.fshow (TxsDefs.view procInst) ++ "!"])
      [] -> return (Left ["Could not find model " ++ show modelId ++ "!"])
  where
    bla :: Set.Set ChanId.ChanId -> String
    bla cids = "[[ " ++ concatMap (\s -> "{" ++ Text.unpack (ChanId.name s) ++ "}") (Set.toList cids) ++ " ]]"
-- toLPEModel

constructFromRefinedSummandData :: LPEChanMap -> (ChanId.ChanId, [VarId.VarId], TxsDefs.VExpr, LPEParamEqs) -> LPESummand
constructFromRefinedSummandData chanMap (cid, vids, guard, paramEqs) =
    LPESummand { lpeSmdChan = cid
               , lpeSmdVars = vids
               , lpeSmdPriority = False
               , lpeSmdQuiescent = False
               , lpeSmdInvisible = List.null (getChanDataFromChanMap chanMap cid)
               , lpeSmdGuard = guard
               , lpeSmdEqs = paramEqs
               , lpeSmdRelevantParams = Map.keysSet paramEqs
               , lpeSmdDebug = ""
               }
-- constructFromRefinedSummandData

-- Refines summand data by removing multi-channels.
-- Fresh channels are introduced to accomplish this.
-- An LPEChanMap is constructed so that the original multi-channels can be retrieved.
refineSummandData :: [(TxsDefs.ActOffer, LPEParamEqs)] -> IOC.IOC ([(ChanId.ChanId, [VarId.VarId], TxsDefs.VExpr, LPEParamEqs)], LPEChanMap)
refineSummandData summandData = do
    let hiddenVidsPerChan = getHiddenVidsPerChan Map.empty summandData
    chanMap <- Map.fromList <$> Monad.mapM getChanMapEntry (Map.toList hiddenVidsPerChan)
    refinedSummandData <- Monad.mapM (getRefinedSummandData hiddenVidsPerChan chanMap) summandData
    return (refinedSummandData, chanMap)
  where
    -- Iterates over all summand data.
    -- Per combination of channels, it gathers all hidden variables that are attached to them.
    -- Note that hidden variables are always fresh (courtesy of ensureFreshCommVars)!
    getHiddenVidsPerChan :: Map.Map [ChanId.ChanId] [VarId.VarId] -> [(TxsDefs.ActOffer, LPEParamEqs)] -> Map.Map [ChanId.ChanId] [VarId.VarId]
    getHiddenVidsPerChan soFar [] = soFar
    getHiddenVidsPerChan soFar ((x, _):xs) =
        let cids = Set.toList (getActOfferChans x) in
        let hiddenVids = Set.toList (TxsDefs.hiddenvars x) in
          case soFar Map.!? cids of
            Just vids -> getHiddenVidsPerChan (Map.insert cids (vids ++ hiddenVids) soFar) xs
            Nothing -> getHiddenVidsPerChan (Map.insert cids hiddenVids soFar) xs
    -- getHiddenVidsPerChan
    
    -- Per combination of channels, create a fresh channel to replace it.
    -- The sort of the fresh channel is constructed by
    --  - merging the sorts of the original channels;
    --  - adding the sorts of all hidden variables (throughout the LPE).
    getChanMapEntry :: ([ChanId.ChanId], [VarId.VarId]) -> IOC.IOC (ChanId.ChanId, LPEChanSignature)
    getChanMapEntry (cids, hiddenVids) = do
        let visibleSorts = concatMap ChanId.chansorts cids
        let hiddenSorts = map SortOf.sortOf hiddenVids
        freshChan <- createFreshChanFromChansAndSorts cids hiddenSorts
        return (freshChan, (cids, visibleSorts, hiddenSorts))
    -- getChanMapEntry
    
    -- Per summand, replace the combination of channels with the corresponding fresh channel.
    -- The channel requires a certain number of communication variables:
    --  - Communication variables are copied from the summand's ActOffer as much as possible, both visible ones and hidden ones;
    --  - Hidden communication variables that are not in the summand's ActOffer are replaced by fresh ones.
    getRefinedSummandData :: Map.Map [ChanId.ChanId] [VarId.VarId] -> LPEChanMap -> (TxsDefs.ActOffer, LPEParamEqs) -> IOC.IOC (ChanId.ChanId, [VarId.VarId], TxsDefs.VExpr, LPEParamEqs)
    getRefinedSummandData hiddenVidsPerChan chanMap (actOffer, paramEqs) = do
        let cids = Set.toList (getActOfferChans actOffer)
        let allHiddenVids = hiddenVidsPerChan Map.! cids
        newHiddenVids <- Monad.mapM (makeFreshUnlessInSet (TxsDefs.hiddenvars actOffer)) allHiddenVids
        let matches = Map.filter (\(k, _, _) -> k == cids) chanMap
        let freshChanId = head (Map.keys matches)
        return (freshChanId, getVisibleVars actOffer ++ newHiddenVids, TxsDefs.constraint actOffer, paramEqs)
    -- getRefinedSummandData
    
    getVisibleVars :: TxsDefs.ActOffer -> [VarId.VarId]
    getVisibleVars actOffer =
        let chanOffers = concatMap TxsDefs.chanoffers (Set.toList (TxsDefs.offers actOffer)) in
          concatMap getVisibleVar chanOffers
    -- getVisibleVars
    
    getVisibleVar :: TxsDefs.ChanOffer -> [VarId.VarId]
    getVisibleVar (TxsDefs.Quest vid) = [vid]
    getVisibleVar _ = []
    
    makeFreshUnlessInSet :: Set.Set VarId.VarId -> VarId.VarId -> IOC.IOC VarId.VarId
    makeFreshUnlessInSet varSet vid =
        if Set.member vid varSet
        then return vid
        else createFreshVarFromVar vid
    -- makeFreshUnlessInSet
-- getChanMapFromSummandData

-- Ensures that all communication variables are fresh.
-- In other words: summands may not share communication variables!
-- Also removes occurrences of invisible channels (ISTEP).
ensureFreshCommVars :: [(TxsDefs.ActOffer, LPEParamEqs)] -> IOC.IOC [(TxsDefs.ActOffer, LPEParamEqs)]
ensureFreshCommVars = Monad.mapM ensureInSummand
  where
    ensureInSummand :: (TxsDefs.ActOffer, LPEParamEqs) -> IOC.IOC (TxsDefs.ActOffer, LPEParamEqs)
    ensureInSummand (actOffer, paramEqs) = do
        perOffer <- Monad.mapM ensureInOffer (Set.toList (TxsDefs.offers actOffer))
        let (newOffers, freshVizVidSubst) = (concatMap fst perOffer, Map.fromList (concatMap snd perOffer))
        freshHiddenVidSubst <- createFreshVars (TxsDefs.hiddenvars actOffer)
        let freshVidSubst = Map.map ValExpr.cstrVar (Map.union freshVizVidSubst freshHiddenVidSubst)
        newConstraint <- doBlindSubst freshVidSubst (TxsDefs.constraint actOffer)
        newParamEqs <- doBlindParamEqsSubst freshVidSubst paramEqs
        return (TxsDefs.ActOffer { TxsDefs.offers = Set.fromList newOffers
                                 , TxsDefs.hiddenvars = Set.fromList (Map.elems freshHiddenVidSubst)
                                 , TxsDefs.constraint = newConstraint
                                 }, newParamEqs)
    -- ensureInSummand
    
    ensureInOffer :: TxsDefs.Offer -> IOC.IOC ([TxsDefs.Offer], [(VarId.VarId, VarId.VarId)])
    ensureInOffer offer =
        if isInvisibleOffer offer
        then return ([], []) -- Invisible offers cannot have communication variables.
                             -- Furthermore, we argue that A|ISTEP == A.
                             -- If a summand uses exactly ISTEP, ActOffer simply contains no Offers.
        else do perChanOffer <- Monad.mapM ensureInChanOffer (TxsDefs.chanoffers offer)
                let (newChanOffers, freshVidSubst) = (concatMap fst perChanOffer, concatMap snd perChanOffer)
                return ([offer { TxsDefs.chanoffers = newChanOffers }], freshVidSubst)
    -- ensureInOffer
    
    ensureInChanOffer :: TxsDefs.ChanOffer -> IOC.IOC ([TxsDefs.ChanOffer], [(VarId.VarId, VarId.VarId)])
    ensureInChanOffer (TxsDefs.Quest vid) = do
        freshVid <- createFreshVarFromVar vid
        return ([TxsDefs.Quest freshVid], [(vid, freshVid)])
    ensureInChanOffer _ = return ([], []) -- (Should not happen, ensured by getValidatedSummandData.)
-- ensureFreshCommVars

-- Only keeps data from summands that use a channel that is in the given channel alphabet.
filterSummandDataByChanAlphabet :: Set.Set (Set.Set ChanId.ChanId) -> [(TxsDefs.ActOffer, LPEParamEqs)] -> [(TxsDefs.ActOffer, LPEParamEqs)]
filterSummandDataByChanAlphabet chanAlphabet = filter (\(a, _) -> isActOfferInChanAlphabet chanAlphabet a)

-- Extracts core data from a hierarchical process expression.
-- That is, it adds one entry per summand that contains the summand's action offer and process instantiation.
-- It also performs validation of the LPEs format.
getValidatedSummandData :: TxsDefs.TxsDefs -> TxsDefs.ProcId -> TxsDefs.ProcDef -> TxsDefs.BExpr -> Either [String] [(TxsDefs.ActOffer, LPEParamEqs)]
getValidatedSummandData tdefs expectedProcId expectedProcDef@(TxsDefs.ProcDef defChanIds params _body) expr =
    case TxsDefs.view expr of
      TxsDefs.Choice choices -> concatEitherList (map (getValidatedSummandData tdefs expectedProcId expectedProcDef) (Set.toList choices))
      TxsDefs.ActionPref actOffer procInst ->
          case TxsDefs.view procInst of
            TxsDefs.ProcInst procId chanIds paramValues
                | procId /= expectedProcId -> Left ["Expected instantiation of " ++ show expectedProcId ++ ", found instantiation of " ++ show procId ++ "!"]
                | chanIds /= defChanIds -> Left ["Signature mismatch in channels, found " ++ TxsShow.fshow (TxsDefs.view procInst) ++ "!"]
                | otherwise -> case getParamEqs tdefs "process instantiation" params paramValues of
                                 Left msgs -> Left msgs
                                 Right paramEqs -> let msgs = validateActOffer actOffer in
                                                     if null msgs
                                                     then Right [(actOffer, paramEqs)]
                                                     else Left msgs
            _ -> Left ["Expected process instantiation, found " ++ TxsShow.fshow (TxsDefs.view procInst) ++ "!"]
      _ -> Left ["Expected choice or action prefix, found " ++ TxsShow.fshow (TxsDefs.view expr) ++ "!"]
  where
    validateActOffer :: TxsDefs.ActOffer -> [String]
    validateActOffer actOffer =
        let offers = Set.toList (TxsDefs.offers actOffer) in
          concatMap validateChanOffer (concatMap TxsDefs.chanoffers offers)
    -- validateActOffer
    
    validateChanOffer :: TxsDefs.ChanOffer -> [String]
    validateChanOffer (TxsDefs.Quest _) = []
    validateChanOffer other = ["Expected communication variable, found " ++ show other ++ "!"]
-- getValidatedSummandData

-- Constructs an LPEParamEqs from given input.
-- Occurrences of ANY are replaced by default values of the appropriate Sort.
-- Resulting LPEParamEqs as a whole is also type-checked.
getParamEqs :: TxsDefs.TxsDefs -> String -> [VarId.VarId] -> [TxsDefs.VExpr] -> Either [String] LPEParamEqs
getParamEqs tdefs location params paramValues =
    let msgs = validateSortList location (map SortOf.sortOf params) (map SortOf.sortOf paramValues) in
      if null msgs
      then let newParamValues = map (any2defaultValue tdefs) paramValues in
             Right (Map.fromList (zip params newParamValues))
      else Left msgs
-- getParamEqs

-- Constructs a process expression and a process definition from an LPEModel (unless there is a problem).
-- The process expression creates an instance of the process definition.
lpe2model :: LPE -> IOC.IOC TxsDefs.ModelId
lpe2model lpe = do
    let orderedChanParams = Set.toList (revertSimplChanIdsWithChanMap (lpeChanMap lpe) (lpeChanParams lpe))
    let orderedDataParams = Map.keys (lpeInitEqs lpe)
    
    -- Create a new process:
    newProcUnid <- IOC.newUnid
    let newProcId = TxsDefs.ProcId { ProcId.name = Text.pack (Text.unpack (lpeName lpe) ++ "Process")
                                   , ProcId.unid = newProcUnid
                                   , ProcId.procchans = map (ProcId.ChanSort . ChanId.chansorts) orderedChanParams
                                   , ProcId.procvars = map VarId.varsort orderedDataParams
                                   , ProcId.procexit = ProcId.NoExit }
    let newProcInit = TxsDefs.procInst newProcId orderedChanParams (paramEqsLookup orderedDataParams (lpeInitEqs lpe))
    let newProcBody = TxsDefs.choice (Set.fromList (map (summandToBExpr newProcId orderedChanParams orderedDataParams) (Set.toList (lpeSummands lpe))))
    let newProcDef = TxsDefs.ProcDef orderedChanParams orderedDataParams newProcBody
    
    -- Create a new model:
    newModelId <- getModelIdFromName (lpeName lpe)
    let inChans = Set.toList (Set.map (revertSimplChanIdWithChanMap (lpeChanMap lpe)) (lpeInChans lpe))
    let outChans = Set.toList (Set.map (revertSimplChanIdWithChanMap (lpeChanMap lpe)) (lpeOutChans lpe))
    let newModelDef = TxsDefs.ModelDef inChans outChans (lpeSplSyncs lpe) newProcInit
    
    -- Add process and model:
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    IOC.modifyCS (\st -> st { IOC.tdefs = tdefs { TxsDefs.procDefs = Map.insert newProcId newProcDef (TxsDefs.procDefs tdefs)
                                                , TxsDefs.modelDefs = Map.insert newModelId newModelDef (TxsDefs.modelDefs tdefs)
                                                } })
    
    -- Done!
    return newModelId
  where
      -- Constructs a behavioral expression from a summand.
      summandToBExpr :: TxsDefs.ProcId -> [ChanId.ChanId] -> [VarId.VarId] -> LPESummand -> TxsDefs.BExpr
      summandToBExpr lpeProcId orderedChanParams orderedDataParams summand =
          let actOffer = getActOfferFromChanMap (lpeChanMap lpe) (lpeSmdChan summand) (lpeSmdVars summand) (lpeSmdGuard summand) in
          let procInst = TxsDefs.procInst lpeProcId orderedChanParams (paramEqsLookup orderedDataParams (lpeSmdEqs summand)) in
            TxsDefs.actionPref actOffer procInst
-- fromLPEModel

