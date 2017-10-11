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

import ProcId 
import VarId
import BehExprDefs
import ValExprDefs

import qualified Data.Text         as T


type ProcDefs = Map.Map TxsDefs.ProcId TxsDefs.ProcDef

-- ----------------------------------------------------------------------------------------- --
-- preGNF : 
-- ----------------------------------------------------------------------------------------- --


preGNF :: ProcId -> [ProcId] -> ProcDefs -> ProcDefs
preGNF procId preGNFTranslatedProcDefs procDefs =    
    -- decompose the ProcDef of ProcId
    let ProcDef chansDef paramsDef bexpr = case Map.lookup procId procDefs of
                                                 Just procDef   -> procDef
                                                 Nothing                     -> error "called preGNF with a non-existing procId" in
    -- remember the current ProcId to avoid recursive loops translating the same ProcId again                                         
    let preGNFTranslatedProcDefs' = preGNFTranslatedProcDefs ++ [procId] in                                              
    -- translate each choice separately
    case bexpr of 
            (Choice bexprs) -> let  (bexprs', procDefs') = applyPreGNFBexpr bexprs 1 [] preGNFTranslatedProcDefs' procDefs
                                    procDef' = ProcDef chansDef paramsDef (Choice bexprs') in
                               Map.insert procId procDef' procDefs'

            bexpr -> let    (bexpr', procDefs') = preGNFBExpr bexpr 1 [] procId preGNFTranslatedProcDefs' procDefs 
                            procDef' = ProcDef chansDef paramsDef bexpr' in
                     Map.insert procId procDef' procDefs'

    where
        -- apply preGNFBExpr to each choice and collect all intermediate results (single bexprs)
        applyPreGNFBexpr :: [BExpr] -> Int -> [BExpr] -> [ProcId] -> ProcDefs -> ([BExpr], ProcDefs)
        applyPreGNFBexpr [] cnt results preGNFTranslatedProcDefs procDefs = (results, procDefs)
        applyPreGNFBexpr (bexpr:bexprs) cnt results preGNFTranslatedProcDefs procDefs = 
                let (bexpr', procDefs') = preGNFBExpr bexpr cnt [] procId preGNFTranslatedProcDefs procDefs in 
                applyPreGNFBexpr bexprs (cnt+1) (results ++ [bexpr']) preGNFTranslatedProcDefs procDefs'



preGNFBExpr :: BExpr -> Int -> [VarId] -> ProcId -> [ProcId] -> ProcDefs -> (BExpr, ProcDefs)
preGNFBExpr bexpr choiceCnt freeVarsInScope procId preGNFTranslatedProcDefs procDefs = 
    case bexpr of 
        Stop    -> (Stop, procDefs)
        --(ActionPref actOffer Stop) -> (bexpr, procDefs)
        (ActionPref actOffer bexpr') -> let freeVarsInScope' = freeVarsInScope ++ extractVars(actOffer)
                                            (bexpr'', procDefs') = preGNFBExpr bexpr' choiceCnt freeVarsInScope' procId preGNFTranslatedProcDefs procDefs in
                                        (ActionPref actOffer bexpr'', procDefs')
        (ProcInst procId chansInst paramsInst) ->   if (notElem procId preGNFTranslatedProcDefs)
                                                        then    -- recursively translate the called ProcDef
                                                                let procDefs' = preGNF procId preGNFTranslatedProcDefs procDefs in
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
                                name' = T.append (ProcId.name procId) (T.pack ("$" ++ show choiceCnt)) 
                                procId' = procId { ProcId.name = name'}
                            -- create ProcInst, translate params to VExprs 
                                paramsDef' = map ValExpr (map Vvar paramsDef)
                                paramsFreeVars = map ValExpr (map Vvar freeVarsInScope)
                                procInst' = ProcInst procId' chansDef (paramsDef' ++ paramsFreeVars) 
                            -- put created ProcDefs in the ProcDefs
                                procDefs' = Map.insert procId' procDef' procDefs
                            -- recursively translate the created ProcDef
                                procDefs'' = preGNF procId' preGNFTranslatedProcDefs procDefs' in
                            (procInst', procDefs'')

        (Parallel syncChans operands) -> error "parallel not yet implemented"        
        other -> error "unexpected type of bexpr"

    where 
        extractVars :: ActOffer -> [VarId]
        extractVars actOffer = let  set = offers actOffer in
                               Set.foldr collect [] set 

        collect :: Offer -> [VarId] -> [VarId]
        collect Offer{chanoffers = coffers} varIds = foldr extractVarIds [] coffers 

        extractVarIds :: ChanOffer -> [VarId] -> [VarId]
        extractVarIds (Quest varId) varIds  = (varId:varIds)
        extractVarIds _ varIds              = varIds


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
