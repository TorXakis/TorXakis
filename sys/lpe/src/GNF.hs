{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module GNF
( gnf )

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import TxsDefs
import qualified TxsUtils
import LPEHelpers
import PreGNF

import ProcId
import VarId
import BehExprDefs
import ValExpr

import qualified Data.Text         as T

import Expand (relabel)
import Subst

import TranslatedProcDefs

type ProcDefs = Map.Map TxsDefs.ProcId TxsDefs.ProcDef

-- ----------------------------------------------------------------------------------------- --
-- GNF :
-- ----------------------------------------------------------------------------------------- --

emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
                                             , TranslatedProcDefs.lGNF = []
                                             , TranslatedProcDefs.lLPE = [] }

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
                                                            bexprSubstituted = Subst.subst parammap bexprRelabeled in
                                                            (extractSteps bexprSubstituted, procDefs')



        _ -> error "not implemented"


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
