{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module LPE
( lpeTransform,
  lpe
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import TxsDefs
import qualified TxsUtils
import LPEHelpers
import GNF
import ConstDefs

import ProcId
import ChanId
import VarId
import BehExprDefs
import SortId
import StdTDefs (stdSortTable)

import qualified Data.Text         as T

import Expand (relabel)
import Subst
import ValExpr

import TranslatedProcDefs

import Debug.Trace

type ProcDefs = Map.Map TxsDefs.ProcId TxsDefs.ProcDef

type Proc = (ProcId, [ChanId])
type PCMapping = Map.Map Proc Integer
type ProcToParams = Map.Map Proc [VarId]

type ChanMapping = Map.Map ChanId ChanId
type ParamMapping = Map.Map VarId VExpr

-- ----------------------------------------------------------------------------------------- --
-- LPE :
-- ----------------------------------------------------------------------------------------- --

intSort = case Map.lookup (T.pack "Int") stdSortTable of
                    Just sort   -> sort
                    Nothing     -> error "LPE module: could not find standard IntSort"



emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
                                             , TranslatedProcDefs.lGNF = []
                                             , TranslatedProcDefs.lLPE = [] }



-- wrapper around lpe function, returning only the relevant ProcDef instead of all ProcDefs
lpeTransform :: BExpr -> ProcDefs -> Maybe (BExpr, TxsDefs.ProcDef)
lpeTransform procInst procDefs = let (procInst', procDefs') = lpe procInst emptyTranslatedProcDefs procDefs
                                     ProcInst procIdInst chansInst paramsInst = procInst'
                                     ProcDef chans params bexpr = case Map.lookup procIdInst procDefs' of
                                                                   Just procDef   -> procDef
                                                                   Nothing        -> error "lpeTransform: could not find the given procId"


                                     -- rename ProcId P to LPE_<P>
                                     -- put new ProcId in the procInst
                                     procIdName' = T.pack $ "LPE_" ++ (T.unpack (ProcId.name procIdInst))
                                     procIdInst' = procIdInst { ProcId.name = procIdName'}
                                     procInst'' = ProcInst procIdInst' chansInst paramsInst

                                     -- put new ProcId in each step
                                     steps = map (substituteProcId procIdInst procIdInst') (extractSteps bexpr)
                                     procDef = ProcDef chans params (wrapSteps steps) in
                                 Just (procInst'', procDef)
    where
        substituteProcId :: ProcId -> ProcId -> BExpr -> BExpr
        substituteProcId orig new Stop = Stop
        substituteProcId orig new (ActionPref actOffer (ProcInst procId chansInst paramsInst)) =
          if (procId == orig)
              then ActionPref actOffer (ProcInst new chansInst paramsInst)
              else error "Found a different ProcId, thus the given BExpr is probably not in LPE format"

lpe :: BExpr -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpe procInst@(ProcInst procIdInst chansInst paramsInst) translatedProcDefs procDefs =
    let -- remember the current ProcId to avoid recursive loops translating the same ProcId again
        translatedProcDefs' = translatedProcDefs { lLPE = (lLPE translatedProcDefs) ++ [procIdInst]}
        -- first translate to GNF
        procDefs' = gnf procIdInst translatedProcDefs' procDefs
        -- decompose translated ProcDef
        ProcDef chansDef paramsDef bexpr = case Map.lookup procIdInst procDefs' of
                             Just procDef   -> procDef
                             Nothing        -> error "LPE: could not find given procId (should be impossible)"

        accuInit = [(procIdInst, chansDef)]
        calledProcs = calledProcDefs procDefs' accuInit (extractSteps bexpr)

        -- create program counter mapping
        pcName = "pc$" ++ (T.unpack (ProcId.name procIdInst))
        varIdPC = VarId (T.pack pcName) 0 intSort
        pcMapping = Map.fromList $ zip calledProcs [i | i <- [0..]]

        (steps, params, procToParams) = translateProcs calledProcs varIdPC pcMapping procDefs'

        -- create the new ProcId, ProcInst, ProcDef
        -- TODO: unid change
        nameNew = (T.unpack (ProcId.name procIdInst))
        paramsNew = (varIdPC : params)
        procIdNew = procIdInst { ProcId.name = T.pack nameNew
                               , ProcId.procchans = chansDef
                               , ProcId.procvars = paramsNew}

        -- TODO: use predefined intSort
        intSort = SortId {  SortId.name = T.pack "Int"
                          , SortId.unid = 1}

        -- update the ProcInsts in the steps
        steps' = map (stepsUpdateProcInsts calledProcs procToParams pcMapping procIdNew) steps

        procDefLpe = ProcDef chansDef paramsNew (wrapSteps steps')
        procDefs'' = Map.insert procIdNew procDefLpe procDefs'

        -- update the ProcInst to the new ProcDef
        procInstLPE = updateProcInst procInst procIdNew calledProcs
        in
    (procInstLPE, procDefs'')
    where
        -- recursively collect all (ProcId, Channels)-combinations that are called
        calledProcDefs :: ProcDefs -> [Proc] -> [BExpr] -> [Proc]
        calledProcDefs procDefs accu bexprs = foldl (processStep procDefs) accu bexprs
          where
            processStep :: ProcDefs -> [Proc] -> BExpr -> [Proc]
            -- case bexpr == A >-> P'[]()
            processStep procDefs accu (ActionPref actOffer procInst@(ProcInst procIdInst chansInst paramsInst)) =
              if (elem (procIdInst, chansInst) accu)
                then accu
                else let -- add combination to accu
                         accu' = accu ++ [(procIdInst, chansInst)]
                         -- decompose ProcDef
                         ProcDef chansDef paramsDef bexprDef = case Map.lookup procIdInst procDefs of
                                     Just procDef   -> procDef
                                     Nothing        -> error "LPE: could not find given procId (should be impossible)"
                         -- instantiate bexpr with channels of ProcInst
                         chanmap = Map.fromList (zip chansDef chansInst)
                         bexprRelabeled = relabel chanmap bexprDef
                         -- go through steps recursively
                         accu'' = calledProcDefs procDefs accu' (extractSteps bexprRelabeled) in
                     accu''

            -- case bexpr == A >-> STOP: nothing to collect
            processStep procDefs proc bexpr = proc

        -- translate all Procs (procId, channels)-combination
        translateProcs :: [Proc] -> VarId -> PCMapping -> ProcDefs -> ([BExpr], [VarId], ProcToParams)
        translateProcs [] _ _ _ = ([], [], Map.fromList [])
        translateProcs (currentProc@(procId, chans):procss) varIdPC pcMapping procDefs =
            let -- decompose translated ProcDef
                ProcDef chansDef paramsDef bexprDef = case Map.lookup procId procDefs of
                                                         Just procDef   -> procDef
                                                         Nothing        -> error $ "\ntranslateProcs: could not find given procId (should be impossible)" ++ show procId ++
                                                                                    "\nprocDefs: " ++ show procDefs
                steps = extractSteps bexprDef

                -- prepare channel instantiation
                chanMap = Map.fromList (zip chansDef chans)
                -- prepare param renaming (uniqueness)
                chanNames = map (\chanId -> "$" ++ T.unpack (ChanId.name chanId) ) chans
                prefix = T.unpack (ProcId.name procId) ++ concat chanNames ++ "$"
                -- prefix the params and wrap them as VExpr just to be able to use the substitution function later
                prefixedParams = map cstrVar $ map (prefixVarId prefix) paramsDef
                paramMap = Map.fromList $ zip paramsDef prefixedParams

                -- collect prefixed params for later usage
                paramsPrefixed = map (prefixVarId prefix) paramsDef

                pcValue = case Map.lookup currentProc pcMapping of
                             Just i   -> i
                             Nothing  -> error "translateProcs: could not find the pcValue for given proc (should be impossible)"

                steps' = map (lpeBExpr chanMap paramMap varIdPC pcValue) steps
                steps'' = filter (not . ((==) Stop)) steps'       -- filter out the Stops
                -- recursion
                (stepsRec, paramsRec, procToParamsRec) = translateProcs procss varIdPC pcMapping procDefs

                -- add collected prefixed params for later usage
                params = paramsPrefixed ++ paramsRec
                procToParams = Map.insert currentProc paramsPrefixed procToParamsRec in
            (steps'' ++ stepsRec, params, procToParams)

        -- update the original ProcInst, initialise with artifical values
        updateProcInst :: BExpr -> ProcId -> [Proc] -> BExpr
        updateProcInst (ProcInst procIdInst chansInst paramsInst) procIdNew calledProcs =
            let pcValue = cstrConst (Cint 0)
                -- get the params, but leave out the first ones (those of procIdInst itself)
                -- plus an extra one (that of the program counter)
                params = snd $ splitAt ((length paramsInst)+1) (ProcId.procvars procIdNew)
                paramsSorts = map varIdToSort params
                paramsANYs = map (cstrConst . Cany) paramsSorts
                paramsNew = (pcValue : paramsInst) ++ paramsANYs in
            ProcInst procIdNew chansInst paramsNew

        -- update the ProcInsts in the steps to the new ProcId
        stepsUpdateProcInsts :: [Proc] -> ProcToParams -> PCMapping -> ProcId -> BExpr -> BExpr
        stepsUpdateProcInsts procs procToParams pcMap procIdNew (ActionPref actOffer Stop) =
            let -- get the params, but leave out the first one because it's the program counter
                (_:params) = ProcId.procvars procIdNew
                paramsSorts = map varIdToSort params
                paramsANYs = map (cstrConst . Cany) paramsSorts
                paramsInst = (cstrConst (Cint (-1)) : paramsANYs)
                chansInst = ProcId.procchans procIdNew
                procInst = ProcInst procIdNew chansInst paramsInst in
            (ActionPref actOffer procInst)

        stepsUpdateProcInsts procs procToParams pcMap procIdNew bexpr@(ActionPref actOffer procInst@(ProcInst procIdInst chansInst paramsInst)) =
            let -- collect params AND channels from procs in the order they appear in procs
                paramsNew = createParams procs procInst

                pcValue = case Map.lookup (procIdInst, chansInst) pcMap of
                                                             Just pc   -> pc
                                                             Nothing   -> error "stepsUpdateProcInsts: no pc value found for given (ProcId, [ChanId]) (should be impossible)"

                procInst' = ProcInst procIdNew (ProcId.procchans procIdNew) ( cstrConst (Cint pcValue) : paramsNew) in
            (ActionPref actOffer procInst')
            where
                createParams :: [Proc] -> BExpr -> [VExpr]
                createParams [] _ = []
                createParams (proc@(procId, procChans):procs) procInst@(ProcInst procIdInst chansInst paramsInst) =
                    let paramsRec = createParams procs procInst
                        params = if (procId, procChans) == (procIdInst, chansInst)
                                    then paramsInst
                                    else case Map.lookup proc procToParams of
                                             Just ps   -> map cstrVar ps
                                             Nothing   -> error "createParams: no params found for given proc (should be impossible)"
                    in
                    params ++ paramsRec

        stepsUpdateProcInsts _ _ _ _ bexpr = bexpr


        varIdToSort :: VarId -> SortId
        varIdToSort (VarId _ _ sort) = sort


        prefixVarId :: String -> VarId -> VarId
        prefixVarId prefix (VarId name unid sort) = let name' = T.pack $ prefix ++ (T.unpack name) in
                                                    VarId name' unid sort


lpeBExpr :: ChanMapping -> ParamMapping -> VarId -> Integer -> BExpr -> BExpr
lpeBExpr chanMap paramMap varIdPC pcValue Stop = Stop
lpeBExpr chanMap paramMap varIdPC pcValue bexpr =
    let -- instantiate the bexpr
        bexprRelabeled = relabel chanMap bexpr
        -- TODO: properly initialise funcDefs param of subst
        bexprSubstituted = Subst.subst paramMap (Map.fromList []) bexprRelabeled

        -- decompose bexpr, bexpr' can be STOP or ProcInst (distinction later)
        ActionPref actOffer bexpr' = bexprSubstituted

        (offers', constraints', varMap) = translateOffers (Set.toList (offers actOffer))
        -- constraints of offer need to be substituted:
        -- say A?x [x>1], this becomes A?A1 [A1 > 1]
        constraintOfOffer = constraint actOffer

        varMap' = Map.fromList $ map (\(f,s) -> (f, cstrVar s)) varMap

        -- TODO: properly initialise funcDefs param of subst
        constraintOfOffer' = Subst.subst varMap' (Map.fromList []) constraintOfOffer
        constraintPC = cstrEqual (cstrVar varIdPC) (cstrConst (Cint pcValue))
        constraint' = cstrAnd $ Set.fromList (constraintPC : constraintOfOffer' : constraints')

        actOffer' = ActOffer { offers = Set.fromList offers'
                             , constraint = constraint' }


        bexpr'' = case bexpr' of
                    Stop -> Stop
                    -- TODO: properly initialise funcDefs param of subst
                    procInst -> Subst.subst varMap' (Map.fromList []) procInst
        in
    (ActionPref actOffer' bexpr'')

    where
        -- transform actions: e.g. A?x [x == 1] becomes A?A1 [A1 == 1]
        translateOffers :: [Offer] -> ([Offer], [VExpr], [(VarId, VarId)])
        translateOffers [] = ([], [], [])
        translateOffers (offer:offers) =
            let chanId = chanid offer
                chanOffers = chanoffers offer

                chanOffersNumberedSorts = zip3 [n | n <- [1..]] chanOffers (ChanId.chansorts chanId)
                chanName = T.unpack $ ChanId.name chanId
                (chanOffers', constraints, varMap) = translateChanOffers chanOffersNumberedSorts chanName

                offer' = offer { chanoffers = chanOffers'}
                (offersRec, constraintsRec, varMapRec) = translateOffers offers
            in
            ((offer':offersRec), constraints ++ constraintsRec, varMap ++ varMapRec)
            where
                translateChanOffers :: [(Int, ChanOffer, SortId)] -> String -> ([ChanOffer], [VExpr], [(VarId, VarId)])
                translateChanOffers [] _ = ([], [], [])
                translateChanOffers ((i, chanOffer, sort) : chanOffers) chanName =
                    let -- recursion first
                        (chanOffersRec, constraintsRec, varMapRec) = translateChanOffers chanOffers chanName
                        -- transform current chanOffer
                        chanName' = chanName ++ show i
                        -- TODO: unid change
                        varIdChani = VarId (T.pack chanName') 34 sort
                        chanOffer' = Quest varIdChani
                        constraints = case chanOffer of
                                        (Quest varId)  -> constraintsRec
                                        (Exclam vexpr) -> let constraint' = cstrEqual (cstrVar varIdChani) vexpr in
                                                          (constraint':constraintsRec)
                        varMap = case chanOffer of
                                        (Quest varId)   -> ((varId, varIdChani) : varMapRec)
                                        (Exclam vexpr)  -> varMapRec
                         in
                    ((chanOffer':chanOffersRec), constraints, varMap)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
