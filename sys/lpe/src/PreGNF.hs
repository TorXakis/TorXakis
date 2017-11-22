{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module PreGNF
( preGNF )

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import TxsDefs
import qualified TxsUtils
import LPEHelpers

import ProcId
import VarId
import BehExprDefs
import ValExpr

import qualified Data.Text         as T
import TranslatedProcDefs

type ProcDefs = Map.Map TxsDefs.ProcId TxsDefs.ProcDef

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

        (Parallel syncChans operands) -> error "parallel not yet implemented"
        other -> error "unexpected type of bexpr"

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
