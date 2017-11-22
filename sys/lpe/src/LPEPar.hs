{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module LPEPar
( lpePar )

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import TxsDefs
import qualified TxsUtils
import LPEHelpers
import LPE

import ProcId
import ChanId
import VarId
import BehExprDefs
import SortId
import StdTDefs (stdSortTable)

import qualified Data.Text         as T

import Expand (relabel)
import Subst

import TranslatedProcDefs

import LPE

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

--emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
--                                             , TranslatedProcDefs.lGNF = []
--                                             , TranslatedProcDefs.lLPE = [] }


-- we assume that the top level bexpr of the called ProcDef is Parallel
lpePar :: BExpr -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpePar procInst@(ProcInst procIdInst chansInst paramsInst) translatedProcDefs procDefs =
  let -- get and decompose ProcDef and the parallel bexpr
      ProcDef chansDef paramsDef bexpr = case Map.lookup procIdInst procDefs of
                                            Just procDef   -> procDef
                                            Nothing        -> error "lpePar: could not find the given procId"
      Parallel syncChans ops = bexpr
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
            bexpr'' = Subst.subst paramMap bexpr' in
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
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
