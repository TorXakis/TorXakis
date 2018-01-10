{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  LPE
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  carsten.ruetz, jan.tretmans
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module LPE
( ProcDefs
, preGNF
, gnf
, lpeTransform
, lpe
, lpePar
)

where

-- ----------------------------------------------------------------------------------------- --
-- import

import Debug.Trace
import Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Maybe
import           Data.Monoid

import TranslatedProcDefs

import TxsDefs
import ConstDefs
import StdTDefs (stdSortTable)

import ChanId
import ProcId
import SortId
import VarId

import BehExprDefs
import ValExpr
import qualified TxsUtils

import qualified EnvData
import qualified EnvBasic            as EnvB

import Expand (relabel)
import Subst


-- ----------------------------------------------------------------------------------------- --
-- Types :
-- ----------------------------------------------------------------------------------------- --

type ProcDefs = Map.Map ProcId ProcDef

type Proc = (ProcId, [ChanId])
type PCMapping = Map.Map Proc Integer
type ProcToParams = Map.Map Proc [VarId]

type ChanMapping = Map.Map ChanId ChanId
type ParamMapping = Map.Map VarId VExpr

intSort = case Map.lookup (T.pack "Int") stdSortTable of
                    Just sort   -> sort
                    Nothing     -> error "LPE module: could not find standard IntSort"


-- ----------------------------------------------------------------------------------------- --
-- Helpers :
-- ----------------------------------------------------------------------------------------- --

extractVars :: ActOffer -> [VarId]
extractVars actOffer = let  set = offers actOffer in
                       Set.foldr collect [] set
    where
        collect :: Offer -> [VarId] -> [VarId]
        collect Offer{chanoffers = coffers} varIds = foldr extractVarIds [] coffers

        extractVarIds :: ChanOffer -> [VarId] -> [VarId]
        extractVarIds (Quest varId) varIds  = (varId:varIds)
        extractVarIds _ varIds              = varIds



wrapSteps :: [BExpr] -> BExpr
wrapSteps [bexpr] = bexpr
wrapSteps bexprs = Choice bexprs

extractSteps :: BExpr -> [BExpr]
extractSteps (Choice bexprs) = bexprs
extractSteps bexpr = [bexpr]

-- ----------------------------------------------------------------------------------------- --
-- preGNF :
-- ----------------------------------------------------------------------------------------- --

preGNF :: ProcId -> TranslatedProcDefs -> ProcDefs -> ProcDefs
preGNF procId translatedProcDefs procDefs =
    let    -- decompose the ProcDef of ProcId
        ProcDef chansDef paramsDef bexpr = case Map.lookup procId procDefs of
                                                 Just procDef   -> procDef
                                                 Nothing                     -> error "called preGNF with a non-existing procId"
        -- remember the current ProcId to avoid recursive loops translating the same ProcId again
        translatedProcDefs' = translatedProcDefs { lPreGNF = (lPreGNF translatedProcDefs) ++ [procId]} in
    -- translate each choice separately
    case bexpr of
            (Choice bexprs) -> let  (bexprs', procDefs') = applyPreGNFBexpr bexprs 1 [] translatedProcDefs' procDefs
                                    procDef' = ProcDef chansDef paramsDef (Choice bexprs') in
                               Map.insert procId procDef' procDefs'

            bexpr -> let    (bexpr', procDefs') = preGNFBExpr bexpr 1 [] procId translatedProcDefs' procDefs
                            procDef' = ProcDef chansDef paramsDef bexpr' in
                     Map.insert procId procDef' procDefs'

    where
        -- apply preGNFBExpr to each choice and collect all intermediate results (single bexprs)
        applyPreGNFBexpr :: [BExpr] -> Int -> [BExpr] -> TranslatedProcDefs -> ProcDefs -> ([BExpr], ProcDefs)
        applyPreGNFBexpr [] cnt results translatedProcDefs procDefs = (results, procDefs)
        applyPreGNFBexpr (bexpr:bexprs) cnt results translatedProcDefs procDefs =
                let (bexpr', procDefs') = preGNFBExpr bexpr cnt [] procId translatedProcDefs procDefs in
                applyPreGNFBexpr bexprs (cnt+1) (results ++ [bexpr']) translatedProcDefs procDefs'



preGNFBExpr :: BExpr -> Int -> [VarId] -> ProcId -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
preGNFBExpr bexpr choiceCnt freeVarsInScope procId translatedProcDefs procDefs =
    case bexpr of
        Stop    -> (Stop, procDefs)
        (ActionPref actOffer bexpr') -> let freeVarsInScope' = freeVarsInScope ++ extractVars(actOffer)
                                            (bexpr'', procDefs') = preGNFBExpr bexpr' choiceCnt freeVarsInScope' procId translatedProcDefs procDefs in
                                        (ActionPref actOffer bexpr'', procDefs')
        (ProcInst procIdInst chansInst paramsInst) ->   if (notElem procIdInst (lPreGNF translatedProcDefs))
                                                        then    -- recursively translate the called ProcDef
                                                                let procDefs' = preGNF procIdInst translatedProcDefs procDefs in
                                                                (bexpr, procDefs')
                                                        else    (bexpr, procDefs)


        (Choice bexprs) ->  -- choice at lower level not allowed

                            -- decompose the ProcDef of ProcId
                            let ProcDef chansDef paramsDef _ = case Map.lookup procId procDefs of
                                                                     Just procDef   -> procDef
                                                                     Nothing        -> error "called preGNFBExpr with a non-existing procId"

                            -- create new ProcDef
                                procDef' = ProcDef chansDef (paramsDef ++ freeVarsInScope) bexpr
                            -- create ProcInst calling that ProcDef
                                name' = T.append (ProcId.name procId) (T.pack ("$pre" ++ show choiceCnt))
                                procId' = procId { ProcId.name = name', ProcId.procvars = (paramsDef ++ freeVarsInScope)}
                            -- create ProcInst, translate params to VExprs
                                paramsDef' = map cstrVar paramsDef
                                paramsFreeVars = map cstrVar freeVarsInScope
                                procInst' = ProcInst procId' chansDef (paramsDef' ++ paramsFreeVars)
                            -- put created ProcDefs in the ProcDefs
                                procDefs' = Map.insert procId' procDef' procDefs
                            -- recursively translate the created ProcDef
                                procDefs'' = preGNF procId' translatedProcDefs procDefs' in
                            (procInst', procDefs'')

        (Parallel syncChans operands) ->
                            -- parallel at lower level not allowed

                            -- decompose the ProcDef of ProcId
                            let ProcDef chansDef paramsDef _ = case Map.lookup procId procDefs of
                                                                     Just procDef   -> procDef
                                                                     Nothing        -> error "called preGNFBExpr with a non-existing procId"

                            -- create new ProcDef
                                procDef' = ProcDef chansDef (paramsDef ++ freeVarsInScope) bexpr
                            -- create new ProcId
                                name' = T.append (ProcId.name procId) (T.pack ("$pre" ++ show choiceCnt))
                                procId' = procId { ProcId.name = name', ProcId.procvars = (paramsDef ++ freeVarsInScope)}
                            -- create ProcInst, translate params to VExprs
                                paramsDef' = map cstrVar paramsDef
                                paramsFreeVars = map cstrVar freeVarsInScope
                                procInst' = ProcInst procId' chansDef (paramsDef' ++ paramsFreeVars)
                            -- put created ProcDefs in the ProcDefs
                                procDefs' = Map.insert procId' procDef' procDefs
                            -- translate the created ProcDef with LPEPar
                                (procInst'', procDefs'') = lpePar procInst' translatedProcDefs procDefs' in
                            (procInst'', procDefs'')

        other -> error "unexpected type of bexpr"


-- ----------------------------------------------------------------------------------------- --
-- GNF :
-- ----------------------------------------------------------------------------------------- --

gnf :: ProcId -> TranslatedProcDefs -> ProcDefs -> ProcDefs
gnf procId translatedProcDefs procDefs =
    let -- first translate to preGNF
        procDefs' = preGNF procId emptyTranslatedProcDefs procDefs
        -- remember the current ProcId to avoid recursive loops translating the same ProcId again
        translatedProcDefs' = translatedProcDefs { lGNF = (lGNF translatedProcDefs) ++ [procId]}

        ProcDef chansDef paramsDef bexpr = case Map.lookup procId procDefs' of
                             Just procDef   -> procDef
                             Nothing        -> error "GNF: could not find given procId (should be impossible)" in
        case bexpr of
            (Choice bexprs) -> let  (steps, procDefs'') = applyGNFBexpr bexprs 1 [] translatedProcDefs' procDefs'
                                    procDef = ProcDef chansDef paramsDef (Choice steps) in
                               Map.insert procId procDef procDefs''

            bexpr -> let    (steps, procDefs'') = gnfBExpr bexpr 1 procId translatedProcDefs' procDefs'
                            procDef = ProcDef chansDef paramsDef (wrapSteps steps) in
                     Map.insert procId procDef procDefs''
    where
        -- apply gnfBExpr to each choice and collect all intermediate results (single bexprs)
        applyGNFBexpr :: [BExpr] -> Int -> [BExpr] -> TranslatedProcDefs -> ProcDefs -> ([BExpr], ProcDefs)
        applyGNFBexpr [] cnt results translatedProcDefs procDefs = (results, procDefs)
        applyGNFBexpr (bexpr:bexprs) cnt results translatedProcDefs procDefs =
                let (steps, procDefs') = gnfBExpr bexpr cnt procId translatedProcDefs procDefs in
                applyGNFBexpr bexprs (cnt+1) (results ++ steps) translatedProcDefs procDefs'


gnfBExpr :: BExpr -> Int -> ProcId -> TranslatedProcDefs -> ProcDefs -> ([BExpr], ProcDefs)
gnfBExpr bexpr choiceCnt procId translatedProcDefs procDefs =
    case bexpr of
        Stop    -> ([Stop], procDefs)

        (ActionPref actOffer Stop) -> ([bexpr], procDefs)

        (ActionPref actOffer (ProcInst procIdInst _ _)) -> if (notElem procIdInst (lGNF translatedProcDefs))
                                                                then    -- recursively translate the called ProcDef
                                                                        let procDefs' = gnf procIdInst translatedProcDefs procDefs in
                                                                        ([bexpr], procDefs')
                                                                else    ([bexpr], procDefs)

        (ActionPref actOffer bexpr') -> let -- multi-action not allowed: split it
                                            -- decompose original ProcDef
                                            ProcDef chansDef paramsDef _ = case Map.lookup procId procDefs of
                                                 Just procDef   -> procDef
                                                 Nothing        -> error "GNF: called with a non-existing procId"

                                            -- create new ProcDef
                                            varsInOffer = extractVars actOffer
                                            procDef = ProcDef chansDef (paramsDef ++ varsInOffer) bexpr'
                                            -- create ProcInst calling that ProcDef
                                            name' = T.append (ProcId.name procId) (T.pack ("$gnf" ++ show choiceCnt))
                                            procId' = procId { ProcId.name = name', ProcId.procvars = (paramsDef ++ varsInOffer)}
                                            -- create ProcInst, translate params to VExprs
                                            paramsDef' = map cstrVar paramsDef
                                            paramsFreeVars = map cstrVar varsInOffer
                                            procInst' = ProcInst procId' chansDef (paramsDef' ++ paramsFreeVars)

                                            -- put created ProcDefs in the ProcDefs
                                            procDefs' = Map.insert procId' procDef procDefs
                                            -- recursively translate the created ProcDef
                                            procDefs'' = gnf procId' translatedProcDefs procDefs' in

                                            -- return bexpr with the original bexpr' replaced with the new ProcInst
                                            ([ActionPref actOffer procInst'], procDefs'')

        (ProcInst procIdInst chansInst paramsInst) ->   let -- direct calls are not in GNF: need to instantiate
                                                            -- translate procIdInst to GNF first
                                                            procDefs' = if (notElem procIdInst (lGNF translatedProcDefs))
                                                                            then    -- recursively translate the called ProcDef
                                                                                    gnf procIdInst translatedProcDefs procDefs
                                                                            else    -- if it has been translated already, leave procDefs as is
                                                                                    procDefs
                                                            -- decompose translated ProcDef
                                                            ProcDef chansDef paramsDef bexprDef = case Map.lookup procIdInst procDefs' of
                                                                 Just procDef   -> procDef
                                                                 Nothing        -> error "GNF: called with a non-existing procId"

                                                            -- instantiate
                                                            -- substitute channels
                                                            chanmap = Map.fromList (zip chansDef chansInst)
                                                            bexprRelabeled = relabel chanmap bexprDef -- trace ("chanmap: " ++ show chanmap) relabel chanmap bexprProcDef
                                                            -- substitute params
                                                            parammap = Map.fromList (zip paramsDef paramsInst)
                                                            -- TODO: initialise funcDefs param properly
                                                            bexprSubstituted = Subst.subst parammap (Map.fromList []) bexprRelabeled in
                                                            (extractSteps bexprSubstituted, procDefs')



        _ -> error "not implemented"



-- ----------------------------------------------------------------------------------------- --
-- lpePar :
-- ----------------------------------------------------------------------------------------- --

-- we assume that the top level bexpr of the called ProcDef is Parallel
lpePar :: BExpr -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpePar procInst@(ProcInst procIdInst chansInst paramsInst) translatedProcDefs procDefs =
  let -- get and decompose ProcDef and the parallel bexpr
      ProcDef chansDef paramsDef bexpr = case Map.lookup procIdInst procDefs of
                                            Just procDef   -> procDef
                                            Nothing        -> error "lpePar: could not find the given procId"
      Parallel syncChans ops = bexpr

      -- translate the operands to LPE first
      -- collect (in accu) per operand: translated steps, the generated procInst
      --  and a changed mapping of procDefs
      (_, stepsOpParams, paramsInsts, procDefs') = foldl translateOperand (1, [],[], procDefs) ops


      -- create a new ProcId
      chansDefPAR = chansDef
      paramsDefPAR = concat $ map snd stepsOpParams
      procIdPAR = procIdInst { ProcId.procchans = chansDefPAR,
                                ProcId.procvars = paramsDefPAR}

      -- combine the steps of all operands according to parallel semantics
      -- stepOpParams is a list of pairs, one pair for each operand:
      --    pair = (steps, paramsDef)
      (stepsPAR, _) = foldr1 (combineSteps syncChans procIdPAR chansDefPAR) stepsOpParams

      -- create a new ProcId, ProcDef, ProcInst
      procDefPAR = ProcDef chansDefPAR paramsDefPAR (wrapSteps stepsPAR)
      procDefsPAR = Map.insert procIdPAR procDefPAR procDefs'
      procInstPAR = ProcInst procIdPAR chansInst paramsInsts
  in
  (procInstPAR, procDefsPAR)
    where
      -- takes two operands (steps and paramsDef) and combines them according to parallel semantics
      combineSteps :: [ChanId] -> ProcId -> [ChanId] -> ([BExpr], [VarId]) -> ([BExpr], [VarId]) ->  ([BExpr], [VarId])
      combineSteps syncChans procIdPAR chansDefPAR (stepsL, opParamsL) (stepsR, opParamsR) =
        let -- first only steps of the left operand
            stepsLvalid = filter (isValidStep syncChans) stepsL
            stepsL' = map (updateProcInstL opParamsR procIdPAR chansDefPAR) stepsLvalid
            -- second only steps of the right operand
            stepsRvalid = filter (isValidStep syncChans) stepsR
            stepsR' = map (updateProcInstR opParamsL procIdPAR chansDefPAR) stepsRvalid
            -- finally steps of both operands
            -- stepCombinations :: [(BExpr, BExpr)]
            stepCombinations = [(l,r) | l <- stepsL, r <- stepsR]
            -- stepCombinationsValid :: [(BExpr, BExpr)]
            stepCombinationsValid = filter (isValidStepCombination syncChans) stepCombinations
            stepsLR = map (mergeStepsLR procIdPAR chansDefPAR opParamsL opParamsR) stepCombinationsValid
        in
        -- optimize complexity of this: might be MANY steps...
        (stepsL' ++ stepsR' ++ stepsLR, opParamsL ++ opParamsR)
        where
          mergeStepsLR :: ProcId -> [ChanId] -> [VarId] -> [VarId] -> (BExpr, BExpr) -> BExpr
          mergeStepsLR procIdPAR chansDefPar opParamsL opParamsR (stepL, stepR) =
            let -- decompose steps
                ActionPref ActOffer{offers=offersL, constraint=constraintL} (ProcInst procIdL chansL paramsL) = stepL
                ActionPref ActOffer{offers=offersR, constraint=constraintR} (ProcInst procIdR chansR paramsR) = stepR

                -- combine action offers
                --  union of offers, concatenation of constraints
                offersLR = Set.union offersL offersR
                constraintLR = cstrAnd (Set.fromList [constraintL, constraintR])

                -- new ActOffers and ProcInst
                actOfferLR = ActOffer { offers = offersLR,
                                        constraint = constraintLR}
                procInstLR = ProcInst procIdPAR chansDefPar (paramsL ++ paramsR)
            in
            ActionPref actOfferLR procInstLR

          -- check if given step can be executed by itself according to parallel semantics
          isValidStep :: [ChanId] -> BExpr -> Bool
          isValidStep syncChans (ActionPref ActOffer{offers=os} _) =
            let -- extract all chanIds from the offers in the ActOffer
                chanIds = Set.foldl (\accu offer -> (chanid offer : accu)) [] os in
            -- if there are no common channels with the synchronisation channels: return true
            Set.null $ Set.intersection (Set.fromList syncChans) (Set.fromList chanIds)

          -- check if given step combination can be executed according to parallel semantics
          isValidStepCombination :: [ChanId] -> (BExpr, BExpr) -> Bool
          isValidStepCombination syncChans ((ActionPref ActOffer{offers=offersL} _), (ActionPref ActOffer{offers=offersR} _)) =
            let -- extract all chanIds from the offers in the ActOffer
                chanIdsLSet = Set.fromList $ Set.foldl (\accu offer -> (chanid offer : accu)) [] offersL
                chanIdsRSet = Set.fromList $ Set.foldl (\accu offer -> (chanid offer : accu)) [] offersR
                syncChansSet = Set.fromList $ syncChans

                intersectionLR = Set.intersection chanIdsLSet chanIdsRSet
                intersectionLsyncChans = Set.intersection chanIdsLSet syncChansSet
                intersectionRsyncChans = Set.intersection chanIdsRSet syncChansSet
            in
            (intersectionLR == intersectionLsyncChans) && (intersectionLsyncChans == intersectionRsyncChans)


          updateProcInstL :: [VarId] -> ProcId -> [ChanId] -> BExpr -> BExpr
          updateProcInstL opParamsR procIdPAR chansDefPar (ActionPref actOfferL (ProcInst procIdInstL chansInstL paramsInstL)) =
              ActionPref actOfferL (ProcInst procIdPAR chansDefPar (paramsInstL ++ (map cstrVar opParamsR)))
          updateProcInstR :: [VarId] -> ProcId -> [ChanId] -> BExpr -> BExpr
          updateProcInstR opParamsL procIdPAR chansDefPar (ActionPref actOfferR (ProcInst procIdInstR chansInstR paramsInstR)) =
              ActionPref actOfferR (ProcInst procIdPAR chansDefPar ((map cstrVar opParamsL) ++ paramsInstR))

      -- accu = (opNr, steps, params, procDefs)
      -- foldl : (a -> b -> a) -> a -> [b] -> a
      -- a = (opNr, steps, params, procDefs)
      -- b = BExpr
      -- transform list of bexprs to our big accu combination
      translateOperand :: (Int, [([BExpr], [VarId])], [VExpr], ProcDefs ) -> BExpr -> (Int, [([BExpr], [VarId])], [VExpr], ProcDefs)
      translateOperand (opNr, stepsOpParams, paramsInsts, procDefs) operand =
        let -- translate operand to ProcInst if necessary
            (opProcInst, procDefs') = transformToProcInst operand procIdInst procDefs
            -- translate to lpe
            (procInstLPE@(ProcInst procIdLPE chansInstLPE paramsInstLPE), procDefs'') = lpe opProcInst translatedProcDefs procDefs'
            -- decompose translated ProcDef
            ProcDef chansDef paramsDef bexpr = case Map.lookup procIdLPE procDefs'' of
                                                  Just procDef   -> procDef
                                                  Nothing        -> error "translateOperand: could not find the given procId"

            -- instantiate the channels
            chanmap = Map.fromList (zip chansDef chansInstLPE)
            bexpr' = relabel chanmap bexpr
            -- prefix the params and wrap them as VExpr just to be able to use the substitution function later
            prefix = "op" ++ show opNr ++ "$"
            -- TODO: create new unids as well!
            paramsDefPrefixed = map (prefixVarId prefix) paramsDef
            paramMap = Map.fromList $ zip paramsDef (map cstrVar paramsDefPrefixed)
            -- TODO: properly initialise funcDefs param of subst
            bexpr'' = Subst.subst paramMap (Map.fromList []) bexpr' in
        (opNr+1, stepsOpParams ++ [((extractSteps bexpr''), paramsDefPrefixed)], paramsInsts ++ paramsInstLPE, procDefs'')
          where
            prefixVarId :: String -> VarId -> VarId
            prefixVarId prefix (VarId name unid sort) = let name' = T.pack $ prefix ++ (T.unpack name) in
                                                        VarId name' unid sort

            transformToProcInst :: BExpr -> ProcId -> ProcDefs -> (BExpr, ProcDefs)
            -- if operand is already a ProcInst: no need to change anything
            transformToProcInst procInst@(ProcInst _ _ _) procIdParent procDefs = (procInst, procDefs)
            -- otherwise: create new ProcDef and ProcInst
            transformToProcInst operand procIdParent procDefs =
              let -- decompose parent ProcDef
                  ProcDef chansDef paramsDef bexpr = case Map.lookup procIdParent procDefs of
                                                        Just procDef   -> procDef
                                                        Nothing        -> error "transformToProcInst: could not find the given procId"
                  -- create new ProcDef and ProcInst
                  -- TODO: correct unid
                  procIdNewName = T.pack $ (T.unpack (ProcId.name procIdInst)) ++ "$op" ++ show opNr
                  procIdNew = procIdInst { ProcId.name = procIdNewName }
                  procDefNew = ProcDef chansDef paramsDef operand
                  procDefs' = Map.insert procIdNew procDefNew procDefs

                  procInst' = ProcInst procIdNew chansDef (map cstrVar paramsDef) in
              (procInst', procDefs')


-- ----------------------------------------------------------------------------------------- --
-- LPE :
-- ----------------------------------------------------------------------------------------- --


-- | wrapper around lpe function, returning only the relevant ProcDef instead of all ProcDefs
lpeTransform :: (EnvB.EnvB envb )    -- ^ Monad for unique identifiers and error messages
             => BExpr                -- ^ behaviour expression to be transformed,
                                     --   assumed to be a process instantiation
             -> ProcDefs             -- ^ context of process definitions in which process
                                     --   instantiation is assumed to be defined
             -> envb (Maybe (BExpr, ProcDef))
                                     -- ^ transformed process instantiation with LPE definition
-- template function for lpe
lpeTransform procInst procDefs  =  do
     case procInst of
       ProcInst procid@(ProcId nm uid chids vars ext) chans vexps
         -> case Map.lookup procid procDefs of
              Just procdef
                -> do uid'    <- EnvB.newUnid
                      procid' <- return $ ProcId ("LPE_"<>nm) uid' chids vars ext
                      return $ lpeTransform' procInst procDefs
                               -- Just ( ProcInst procid' chans vexps
                               --      , procdef
                               --      )
              _ -> do EnvB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                     "LPE Transformation: undefined process instantiation" ]
                      return Nothing
       _ -> do EnvB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                              "LPE Transformation: only defined for process instantiation" ]
               return Nothing


-- carsten original function for lpe

lpeTransform' procInst procDefs = let (procInst', procDefs') = lpe procInst emptyTranslatedProcDefs procDefs
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
        constraintsList = constraintOfOffer' : constraints'
        constraintPC = cstrEqual (cstrVar varIdPC) (cstrConst (Cint pcValue))

        -- if there is a constraint other than just the program counter check
        --    i.e. the normal constraint is empty (just True)
        -- then evaluate the program counter constraint first in an IF clause
        --    to avoid evaluation of possible comparisons with ANY in the following constraint
        constraint' = if (constraintsList == [(cstrConst (Cbool True))])
                        then constraintPC
                        else cstrITE constraintPC (cstrAnd (Set.fromList constraintsList)) (cstrConst (Cbool False))

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
                        chanName' = chanName ++ "$" ++ show i
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
