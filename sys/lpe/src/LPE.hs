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
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

module LPE
( ProcDefs
, preGNF
, gnf
, lpeTransform
, lpe
, lpePar
, lpeHide
, preGNFEnable
)
where

import Control.Monad.State

import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Maybe
import qualified Control.Arrow

import TranslatedProcDefs

import TxsDefs
import Constant
import StdTDefs (stdSortTable)

import ChanId
import ProcId
import SortId
import VarId

import ValExpr

import qualified EnvData
import qualified EnvBasic            as EnvB

import Relabel (relabel)
import Subst
import SortOf

-- ----------------------------------------------------------------------------------------- --
-- Types :
-- ----------------------------------------------------------------------------------------- --

type ProcDefs = Map.Map ProcId ProcDef

type Proc = (ProcId, [ChanId])
type PCMapping = Map.Map Proc Integer
type ProcToParams = Map.Map Proc [VarId]

type ChanMapping = Map.Map ChanId ChanId
type ParamMapping = Map.Map VarId VExpr

intSort :: SortId
intSort = fromMaybe (error "LPE module: could not find standard IntSort") (Map.lookup (T.pack "Int") stdSortTable)


-- ----------------------------------------------------------------------------------------- --
-- Helpers :
-- ----------------------------------------------------------------------------------------- --

extractVars :: ActOffer -> [VarId]
extractVars actOffer = let  set = offers actOffer in
                       Set.foldr extractVarIdsOffer [] set

extractVarIdsOffer :: Offer -> [VarId] -> [VarId]
extractVarIdsOffer Offer{chanoffers = choffers} varIds = extractVarIdsChanOffers choffers ++ varIds

extractVarIdsChanOffers :: [ChanOffer] -> [VarId]
extractVarIdsChanOffers = foldr extractVarIdsChanOffer []

extractVarIdsChanOffer :: ChanOffer -> [VarId] -> [VarId]
extractVarIdsChanOffer (Quest varId) varIds  = varId:varIds
extractVarIdsChanOffer _ varIds              = varIds


wrapSteps :: [BExpr] -> BExpr
wrapSteps [bexpr] = bexpr
wrapSteps bexprs = choice $ Set.fromList bexprs

extractSteps :: BExpr -> [BExpr]
extractSteps (TxsDefs.view -> Choice bexprs) = Set.toList bexprs
extractSteps bexpr = [bexpr]

-- ----------------------------------------------------------------------------------------- --
-- preGNF :
-- ----------------------------------------------------------------------------------------- --
--
preGNF :: (EnvB.EnvB envb) => ProcId -> TranslatedProcDefs -> ProcDefs -> envb ProcDefs
preGNF procId translatedProcDefs procDefs' = do
    let -- decompose the ProcDef of ProcId
        ProcDef chansDef paramsDef bexpr = fromMaybe (error $ "called preGNF with a non-existing procId" ++ show procId ++ "\n\n" ++ show procDefs') (Map.lookup procId procDefs')
        -- remember the current ProcId to avoid recursive loops translating the same ProcId again
        translatedProcDefs' = translatedProcDefs { lPreGNF = lPreGNF translatedProcDefs ++ [procId]}
        -- translate each choice separately

    (procDef', procDefs''') <- case TxsDefs.view bexpr of
                                    (Choice bexprs) -> do  (bexprs', procDefs'') <- applyPreGNFBexpr (Set.toList bexprs) 1 [] translatedProcDefs' procDefs'
                                                           let procDef' = ProcDef chansDef paramsDef (choice (Set.fromList bexprs'))
                                                           return (procDef', procDefs'')

                                    _               -> do  (bexpr', procDefs'') <- preGNFBExpr bexpr 1 [] procId translatedProcDefs' procDefs'
                                                           let procDef' = ProcDef chansDef paramsDef bexpr'
                                                           return (procDef', procDefs'')
    return $ Map.insert procId procDef' procDefs'''
    where
        -- apply preGNFBExpr to each choice and collect all intermediate results (single bexprs)
        applyPreGNFBexpr :: (EnvB.EnvB envb) => [BExpr] -> Int -> [BExpr] -> TranslatedProcDefs -> ProcDefs -> envb ([BExpr], ProcDefs)
        applyPreGNFBexpr [] _cnt results _translatedProcDefs procDefs'' = return (results, procDefs'')
        applyPreGNFBexpr (bexpr:bexprs) cnt results translatedProcDefs' procDefs'' = do
                (bexpr', procDefs''') <- preGNFBExpr bexpr cnt [] procId translatedProcDefs' procDefs''
                applyPreGNFBexpr bexprs (cnt+1) (results ++ [bexpr']) translatedProcDefs' procDefs'''



preGNFBExpr :: (EnvB.EnvB envb) => BExpr -> Int -> [VarId] -> ProcId -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)

-- guard: first translate the Bexpr, then prefix the result with the guard again
preGNFBExpr (TxsDefs.view -> Guard vexpr' bexpr) choiceCnt freeVarsInScope procId translatedProcDefs procDefs' = do
    (procInst'', procDefs'') <- preGNFBExpr bexpr choiceCnt freeVarsInScope procId translatedProcDefs procDefs'
    return (TxsDefs.guard vexpr' procInst'', procDefs'')

preGNFBExpr bexpr _ _ _ _ procDefs' | isStop bexpr =
    return (stop, procDefs')

preGNFBExpr (TxsDefs.view -> ActionPref actOffer bexpr') choiceCnt freeVarsInScope procId translatedProcDefs procDefs' = do
    let freeVarsInScope' = freeVarsInScope ++ extractVars actOffer
    (bexpr'', procDefs'') <- preGNFBExpr bexpr' choiceCnt freeVarsInScope' procId translatedProcDefs procDefs'
    return (actionPref actOffer bexpr'', procDefs'')

preGNFBExpr bexpr@(TxsDefs.view -> ProcInst procIdInst _ _) _choiceCnt _freeVarsInScope _procId translatedProcDefs procDefs' =
  if procIdInst `notElem` lPreGNF translatedProcDefs
      then  do -- recursively translate the called ProcDef
               procDefs'' <- preGNF procIdInst translatedProcDefs procDefs'
               return (bexpr, procDefs'')
      else  return (bexpr, procDefs')


preGNFBExpr bexpr@(TxsDefs.view -> Choice{}) choiceCnt freeVarsInScope procId translatedProcDefs procDefs' = do
    -- choice at lower level not allowed
    (procInst'@(TxsDefs.view -> ProcInst procId' _ _), procDefs'') <- preGNFBExprCreateProcDefWithUniqueness bexpr choiceCnt freeVarsInScope procId procDefs'
    -- recursively translate the created ProcDef
    procDefs''' <- preGNF procId' translatedProcDefs procDefs''
    return (procInst', procDefs''')


preGNFBExpr bexpr@(TxsDefs.view -> Parallel{}) choiceCnt freeVarsInScope procId translatedProcDefs procDefs' = do
    -- parallel at lower level not allowed
    (procInst', procDefs'') <- preGNFBExprCreateProcDef bexpr choiceCnt freeVarsInScope procId procDefs'
    -- translate the created ProcDef with LPEPar
    (procInst'', procDefs''') <- lpePar procInst' translatedProcDefs procDefs''
    return (procInst'', procDefs''')


preGNFBExpr bexpr'@(TxsDefs.view -> Hide _hiddenChans _bexpr) choiceCnt freeVarsInScope procId translatedProcDefs procDefs' = do
    -- HIDE at lower level not allowed
    (procInst', procDefs'') <- preGNFBExprCreateProcDef bexpr' choiceCnt freeVarsInScope procId procDefs'
    -- translate the created ProcDef with LPEHide
    lpeHide procInst' translatedProcDefs procDefs'' 


preGNFBExpr bexpr'@(TxsDefs.view -> Enable _bexprL _exitChans _bexprR) choiceCnt freeVarsInScope procId translatedProcDefs procDefs' = do
    -- ENABLE at lower level not allowed
    (procInst', procDefs'') <- preGNFBExprCreateProcDef bexpr' choiceCnt freeVarsInScope procId procDefs'
    -- translate the created ProcDef with LPEEnable
    preGNFEnable procInst' translatedProcDefs procDefs'' 

preGNFBExpr bexpr _ _ _ _ _ =
    error $ "unexpected type of bexpr" ++ show bexpr


preGNFBExprCreateProcDef :: (EnvB.EnvB envb) => BExpr -> Int -> [VarId] -> ProcId -> ProcDefs -> envb(BExpr, ProcDefs)
preGNFBExprCreateProcDef bexpr choiceCnt freeVarsInScope procId procDefs' = do
    unid' <- EnvB.newUnid
    let -- decompose the ProcDef of ProcId
        ProcDef chansDef paramsDef _ = fromMaybe (error "called preGNFBExpr with a non-existing procId") (Map.lookup procId procDefs')

        -- create new ProcDef
        procDef' = ProcDef chansDef (paramsDef ++ freeVarsInScope) bexpr
        -- create new ProcId
        name' = T.append (ProcId.name procId) (T.pack ("$pre" ++ show choiceCnt))
        procId' = procId { ProcId.name = name',
                            ProcId.unid = unid',
                            ProcId.procvars = paramsDef ++ freeVarsInScope}
        -- create ProcInst, translate params to VExprs
        paramsDef' = map cstrVar paramsDef
        paramsFreeVars = map cstrVar freeVarsInScope
        procInst' = procInst procId' chansDef (paramsDef' ++ paramsFreeVars)
        -- put created ProcDefs in the ProcDefs
        procDefs'' = Map.insert procId' procDef' procDefs'
    return (procInst', procDefs'')


preGNFBExprCreateProcDefWithUniqueness :: (EnvB.EnvB envb) => BExpr -> Int -> [VarId] -> ProcId -> ProcDefs -> envb(BExpr, ProcDefs)
preGNFBExprCreateProcDefWithUniqueness bexpr choiceCnt extraParams procId procDefs' = do
    unid' <- EnvB.newUnid
    let 
        -- decompose original ProcDef
        ProcDef chansDef paramsDef _ = fromMaybe (error "called preGNFBExpr with a non-existing procId") (Map.lookup procId procDefs')

        -- create new ProcDef
        prefix = (T.unpack $ ProcId.name procId) ++ ("$pre" ++ show choiceCnt ++ "$")
        -- prefix = "pre" ++ show choiceCnt ++ "$"
        params = paramsDef ++ extraParams
    paramsPrefixed <- mapM (prefixVarId prefix) params

    -- substitute original varIds in bexpr with prefixed (unique) versions
    let varMap = zip params paramsPrefixed
        varMap' = Map.fromList $ map (Control.Arrow.second cstrVar) varMap
        bexpr_substituted = Subst.subst varMap' (Map.fromList []) bexpr

        -- -- need to substitute the standard chanoffer variables with the newly unique variables
        -- --  in constraints and the ProcInst (bepxr)
        procDef = ProcDef chansDef paramsPrefixed bexpr_substituted
        
        -- create ProcInst calling that ProcDef
        name' = T.append (ProcId.name procId) (T.pack ("$pre" ++ show choiceCnt))
        procId' = procId { ProcId.name = name',
                            ProcId.unid = unid',
                            ProcId.procvars = paramsPrefixed}
        -- create ProcInst, translate params to VExprs
        paramsDef' = map cstrVar paramsDef
        paramsFreeVars = map cstrVar extraParams
        procInst' = procInst procId' chansDef (paramsDef' ++ paramsFreeVars)

        -- put created ProcDef in the ProcDefs
        procDefs'' = Map.insert procId' procDef procDefs'

    return (procInst', procDefs'')



-- ----------------------------------------------------------------------------------------- --
-- GNF :
-- ----------------------------------------------------------------------------------------- --

gnf :: (EnvB.EnvB envb) => ProcId -> (Map.Map ProcId [ProcId]) -> TranslatedProcDefs -> ProcDefs -> envb (ProcDefs, Map.Map ProcId [ProcId])
gnf procId gnfTodo translatedProcDefs procDefs' = do
    -- remember the current ProcId to avoid recursive loops translating the same ProcId again
    -- remember the chain of direct calls (without progress, i.e. no ActionPref) to detect loops
    -- remember the ProcIds currently being in GNF translation (also up the tree) to avoid premature substitutions
    let translatedProcDefs' = translatedProcDefs { lGNFdirectcalls = lGNFdirectcalls translatedProcDefs ++ [procId]
                                                 , lGNFinTranslation = lGNFinTranslation translatedProcDefs ++ [procId]}
    -- first translate to preGNF
    procDefs'' <- preGNF procId translatedProcDefs' procDefs'
    
    let ProcDef _chansDef _paramsDef bexpr = fromMaybe (error "GNF: could not find given procId (should be impossible)") (Map.lookup procId procDefs'')

    -- translate steps to GNF 
    applyGNFBexpr (extractSteps bexpr) 1 [] gnfTodo translatedProcDefs' procDefs''                                  
      where
        -- apply gnfBExpr to each step
        applyGNFBexpr :: (EnvB.EnvB envb) => [BExpr] -> Int -> [BExpr] -> (Map.Map ProcId [ProcId]) -> TranslatedProcDefs -> ProcDefs -> envb (ProcDefs, Map.Map ProcId [ProcId])
        applyGNFBexpr [] _cnt results gnfTodo' translatedProcDefs'' procDefs'' = do
            -- base case: translated all steps of the current ProcId
            -- decompose original ProcDef
            let ProcDef chansDef paramsDef _bexpr = fromMaybe (error "GNF: could not find given procId (should be impossible)") (Map.lookup procId procDefs'')

                -- create new ProcDef and insert into ProcDefs
                procDefs''' = Map.insert procId (ProcDef chansDef paramsDef (wrapSteps results)) procDefs''
            
            -- remember that the current ProcId is now translated to GNF
            -- remove current ProcId from ProcIds that are currently being translated
            let translatedProcDefs''' = translatedProcDefs {  lGNF = lGNF translatedProcDefs'' ++ [procId]
                                                            , lGNFinTranslation = List.delete procId (lGNFinTranslation translatedProcDefs'')}
            -- translate to GNF all ProcIds that were waiting for the current ProcId to be finished with GNF translation
            (procDefs'''', gnfTodo'') <- case Map.lookup procId gnfTodo' of
                                            (Just procIdsTodo) -> applyGNF procIdsTodo gnfTodo' translatedProcDefs''' procDefs'''
                                            Nothing -> return (procDefs''', gnfTodo')

            -- remove the current procId from the todo list
            return (procDefs'''', Map.delete procId gnfTodo'')

        applyGNFBexpr (bexpr:bexprs) cnt results gnfTodo' translatedProcDefs'' procDefs'' = do
            -- recursive case: translate one ProcId, use results in the recursion
            (steps, procDefs''', gnfTodo'') <- gnfBExpr bexpr cnt procId gnfTodo' translatedProcDefs'' procDefs''
            -- recursion: translate all other steps
            (procDefs'''', gnfTodo''') <- applyGNFBexpr bexprs (cnt+1) (results ++ steps) gnfTodo'' translatedProcDefs'' procDefs'''
            return (procDefs'''', gnfTodo''')
            

        -- apply GNF sequentially to all given ProcIds
        applyGNF :: (EnvB.EnvB envb) => [ProcId] -> (Map.Map ProcId [ProcId]) -> TranslatedProcDefs -> ProcDefs -> envb (ProcDefs, Map.Map ProcId [ProcId])
        applyGNF [] gnfTodo' _translatedProcDefs procDefs'' = do return (procDefs'', gnfTodo')
        applyGNF (procId':procIds) gnfTodo' translatedProcDefs'' procDefs'' = do 
            -- translate one procDef
            --  reset the gnf loop detection
            let translatedProcDefs''' = translatedProcDefs'' { lGNFdirectcalls = []}
            (procDefs''', gnfTodo'') <- gnf procId' gnfTodo' translatedProcDefs''' procDefs''
            
            -- recursion
            --  put the translated procDef/Id in the translated list: so it doesn't get translated again
            let translatedProcDefs'''' = translatedProcDefs''' { lGNF = lGNF translatedProcDefs ++ [procId']}
            applyGNF procIds gnfTodo'' translatedProcDefs'''' procDefs'''



gnfBExpr :: (EnvB.EnvB envb) => BExpr -> Int -> ProcId -> (Map.Map ProcId [ProcId]) -> TranslatedProcDefs -> ProcDefs -> envb([BExpr], ProcDefs, (Map.Map ProcId [ProcId]))

-- guard: first translate the Bexpr, then prefix the result with the guard again
gnfBExpr (TxsDefs.view -> Guard vexpr' bexpr) choiceCnt procId gnfTodo translatedProcDefs procDefs' = do
    (bexprs, procDefs'', gnfTodo') <- gnfBExpr bexpr choiceCnt procId gnfTodo translatedProcDefs procDefs'
    let bexprs_guarded = map (TxsDefs.guard vexpr') bexprs
    return (bexprs_guarded, procDefs'', gnfTodo')

-- case STOP
gnfBExpr bexpr _choiceCnt _procId gnfTodo _translatedProcDefs procDefs' | isStop bexpr =
      return ([bexpr], procDefs', gnfTodo)

-- case ActionPref -> STOP
gnfBExpr bexpr@(TxsDefs.view -> ActionPref _actOffer bexpr1) _choiceCnt _procId gnfTodo _translatedProcDefs procDefs' | isStop bexpr1 =
      return ([bexpr], procDefs', gnfTodo)

-- case ActionPref -> ProcInst
gnfBExpr bexpr@(TxsDefs.view -> ActionPref _actOffer (TxsDefs.view -> ProcInst procIdInst _ _)) _choiceCnt _procId gnfTodo translatedProcDefs procDefs' =
  if (procIdInst `notElem` lGNF translatedProcDefs) && (procIdInst `notElem` lGNFinTranslation translatedProcDefs)
      then    do  -- recursively translate the called ProcDef
                  -- reset GNF loop detection: we made progress, thus we are breaking a possible chain of direct calls (ProcInsts)
                  let translatedProcDefs' = translatedProcDefs { lGNFdirectcalls = []}
                  (procDefs'', gnfTodo') <- gnf procIdInst gnfTodo translatedProcDefs' procDefs'
                  return ([bexpr], procDefs'', gnfTodo')
      else    return ([bexpr], procDefs', gnfTodo)

-- case ActionPref -> Guard ProcInst
gnfBExpr (TxsDefs.view -> ActionPref actOffer 
                                        bexpr'@(TxsDefs.view -> Guard _vexpr' 
                                                (TxsDefs.view -> ProcInst procIdInst _ _))) choiceCnt procId gnfTodo translatedProcDefs procDefs' = do

    -- create new ProcDef of bexpr'
    (procInst'@(TxsDefs.view -> ProcInst procId' _ _ ), procDefs'') <- gnfBExprCreateProcDefWithUniqueness bexpr' choiceCnt (extractVars actOffer) procId procDefs'

    -- if the ProcId that is being called is still in GNF translation (current GNF translation or up the tree)
    --  then postpone the GNF translation of the newly generated ProcDef
    --      until the ProcId being called has been translated to GNF 
    --      (the newly created ProcDef is of the form [[condition]] -> P() and thus for a GNF translation 
    --          we first need to unfold P() for which we need P() to be translated to GNF already.
    if procIdInst `elem` lGNFinTranslation translatedProcDefs
        then do -- remember the newly created ProcId for later GNF translation
                let procIds = fromMaybe [] (Map.lookup procIdInst gnfTodo)         -- look up the ProcIds already collected
                    gnfTodo' = Map.insert procIdInst (procId':procIds) gnfTodo
                return ([actionPref actOffer procInst'], procDefs'', gnfTodo')                                                    
        else do -- recursively translate the called ProcDef
                -- reset GNF loop detection: we made progress, thus we are breaking a possible chain of direct calls (ProcInsts)
                let translatedProcDefs' = translatedProcDefs { lGNFdirectcalls = []}
                (procDefs''', gnfTodo') <- gnf procIdInst gnfTodo translatedProcDefs' procDefs''
                return ([actionPref actOffer procInst'], procDefs''', gnfTodo')

-- case ActionPref -> "something else"
--  multi-action not allowed: split it
gnfBExpr (TxsDefs.view -> ActionPref actOffer bexpr') choiceCnt procId gnfTodo translatedProcDefs procDefs' = do 
    -- create new ProcDef of bexpr'
    (procInst'@(TxsDefs.view -> ProcInst procId' _ _ ), procDefs'') <- gnfBExprCreateProcDefWithUniqueness bexpr' choiceCnt (extractVars actOffer) procId procDefs'

    -- reset GNF loop detection: we made progress, thus we are breaking a possible chain of direct calls (ProcInsts)
    let translatedProcDefs' = translatedProcDefs { lGNFdirectcalls = []}
    
    -- recursively translate the created ProcDef
    (procDefs''', gnfTodo') <- gnf procId' gnfTodo translatedProcDefs' procDefs''
    -- return bexpr with the original bexpr' replaced with the new ProcInst
    return ([actionPref actOffer procInst'], procDefs''', gnfTodo')


gnfBExpr bexpr@(TxsDefs.view -> ProcInst procIdInst chansInst paramsInst) _choiceCnt procId gnfTodo translatedProcDefs procDefs' =
    -- direct calls are not in GNF: need to instantiate

    -- check if we encounter a loop of direct calls
    --  i.e. a chain of procInsts without any ActionPref, thus we would make no progress
    --  this endless loop cannot be translated to GNF
    if procIdInst `elem` lGNFdirectcalls translatedProcDefs
        then do -- found a loop
                let loop = map ProcId.name $ lGNFdirectcalls translatedProcDefs ++ [procIdInst]
                error ("found GNF loop of direct calls without progress: " ++ show loop)
        else do -- no loop
                -- if the called ProcId is still being translated to GNF: have to come back later
                --      just return the unchanged bexpr
                if procIdInst `elem` lGNFinTranslation translatedProcDefs
                    then do -- add enclosing procId to gnfTodo - if not already present 
                            let procIds = fromMaybe [] (Map.lookup procIdInst gnfTodo)         -- look up the ProcIds already collected
                            if procId `elem` procIds 
                                then do -- procId is already listed for later GNF translation: leave it 
                                        return ([bexpr], procDefs', gnfTodo)
                                else do -- add procId for later GNF translation 
                                        let gnfTodo' = Map.insert procIdInst (procId:procIds) gnfTodo
                                        return ([bexpr], procDefs', gnfTodo')       

                    else do -- ProcId is at least not being translated to GNF still
                            -- check if the called ProcDef has been translated to GNF already
                            (procDefs'', gnfTodo') <- if (procIdInst `notElem` lGNF translatedProcDefs) && (procIdInst `notElem` lGNFinTranslation translatedProcDefs)
                                                            then    -- recursively translate the called ProcDef
                                                                    gnf procIdInst gnfTodo translatedProcDefs procDefs'
                                                            else    -- if it has been translated already, leave procDefs as is
                                                                    return (procDefs', gnfTodo)

                            let -- decompose translated ProcDef
                                ProcDef chansDef paramsDef bexprDef = fromMaybe (error "GNF: called with a non-existing procId") (Map.lookup procIdInst procDefs'')

                                -- instantiate
                                -- substitute channels
                                chanmap = Map.fromList (zip chansDef chansInst)
                                bexprRelabeled = relabel chanmap bexprDef
                                -- substitute params
                                parammap = Map.fromList (zip paramsDef paramsInst)
                                -- TODO: initialise funcDefs param properly
                                bexprSubstituted = Subst.subst parammap (Map.fromList []) bexprRelabeled
                            return (extractSteps bexprSubstituted, procDefs'', gnfTodo')


gnfBExpr (TxsDefs.view -> bexpr')  _ _ _ _ _ =
    error $ "not implemented for this bexpr: " ++ show bexpr'



-- gnfBExprCreateProcDef :: (EnvB.EnvB envb) => BExpr -> Int -> [VarId] -> ProcId -> ProcDefs -> envb(BExpr, ProcDefs)
-- gnfBExprCreateProcDef bexpr choiceCnt extraParams procId procDefs' = do
--     unid' <- EnvB.newUnid
--     let 
--         -- decompose original ProcDef
--         ProcDef chansDef paramsDef _ = fromMaybe (error "GNF: called with a non-existing procId") (Map.lookup procId procDefs')

--         -- create new ProcDef
--         procDef = ProcDef chansDef (paramsDef ++ extraParams) bexpr
        
--         -- create ProcInst calling that ProcDef
--         name' = T.append (ProcId.name procId) (T.pack ("$gnf" ++ show choiceCnt))
--         procId' = procId { ProcId.name = name',
--                             ProcId.unid = unid',
--                             ProcId.procvars = paramsDef ++ extraParams}
--         -- create ProcInst, translate params to VExprs
--         paramsDef' = map cstrVar paramsDef
--         paramsFreeVars = map cstrVar extraParams
--         procInst' = procInst procId' chansDef (paramsDef' ++ paramsFreeVars)

--         -- put created ProcDef in the ProcDefs
--         procDefs'' = Map.insert procId' procDef procDefs'

--     return (procInst', procDefs'')


gnfBExprCreateProcDefWithUniqueness :: (EnvB.EnvB envb) => BExpr -> Int -> [VarId] -> ProcId -> ProcDefs -> envb(BExpr, ProcDefs)
gnfBExprCreateProcDefWithUniqueness bexpr choiceCnt extraParams procId procDefs' = do
    unid' <- EnvB.newUnid
    let 
        -- decompose original ProcDef
        ProcDef chansDef paramsDef _ = fromMaybe (error "GNF: called with a non-existing procId") (Map.lookup procId procDefs')

        -- create new ProcDef
        prefix = (T.unpack $ ProcId.name procId) ++ ("$gnf" ++ show choiceCnt ++ "$")
        -- prefix = "gnf" ++ show choiceCnt ++ "$"
        params = paramsDef ++ extraParams
    paramsPrefixed <- mapM (prefixVarId prefix) params

    -- substitute original varIds in bexpr with prefixed (unique) versions
    let varMap = zip params paramsPrefixed
        varMap' = Map.fromList $ map (Control.Arrow.second cstrVar) varMap
        bexpr_substituted = Subst.subst varMap' (Map.fromList []) bexpr

        -- -- need to substitute the standard chanoffer variables with the newly unique variables
        -- --  in constraints and the ProcInst (bepxr)
        -- let varMap' = Map.fromList $ map (Control.Arrow.second cstrVar) varMap
        --     constraint_substituted = Subst.subst varMap' (Map.fromList []) $ constraint actOffer
        --     bexpr_substituted = Subst.subst varMap' (Map.fromList []) bexpr
        -- return $ actionPref 
        --             actOffer{ offers = Set.fromList os'
        --                     , constraint = constraint_substituted
        --                     , hiddenvars = hidvars} 
        --             bexpr_substituted

        procDef = ProcDef chansDef paramsPrefixed bexpr_substituted
        
        -- create ProcInst calling that ProcDef
        name' = T.append (ProcId.name procId) (T.pack ("$gnf" ++ show choiceCnt))
        procId' = procId { ProcId.name = name',
                            ProcId.unid = unid',
                            ProcId.procvars = paramsPrefixed}
        -- create ProcInst, translate params to VExprs
        paramsDef' = map cstrVar paramsDef
        paramsFreeVars = map cstrVar extraParams
        procInst' = procInst procId' chansDef (paramsDef' ++ paramsFreeVars)

        -- put created ProcDef in the ProcDefs
        procDefs'' = Map.insert procId' procDef procDefs'

    return (procInst', procDefs'')


-- ----------------------------------------------------------------------------------------- --
-- lpePar :
-- ----------------------------------------------------------------------------------------- --

-- we assume that the top level bexpr of the called ProcDef is Parallel
lpePar :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
lpePar (TxsDefs.view -> ProcInst procIdInst chansInst _paramsInst) translatedProcDefs procDefs' = do
    let -- get and decompose ProcDef and the parallel bexpr
      ProcDef chansDef _paramsDef bexpr = fromMaybe (error "lpePar: could not find the given procId") (Map.lookup procIdInst procDefs')
      Parallel syncChans ops = TxsDefs.view bexpr

      -- translate the operands to LPE first
      -- collect (in accu) per operand: translated steps, the generated procInst
      --  and a changed mapping of procDefs
    (_, stepsOpParams, paramsInsts, procDefs'') <- foldM translateOperand (1, [],[], procDefs') ops

    unid' <- EnvB.newUnid
    let -- create a new ProcId
        chansDefPAR = chansDef
        paramsDefPAR = concatMap snd stepsOpParams
        procIdPAR = procIdInst { ProcId.procchans = chansDefPAR,
                                  ProcId.unid = unid',
                                  ProcId.procvars = paramsDefPAR}

        -- combine the steps of all operands according to parallel semantics
        -- stepOpParams is a list of pairs, one pair for each operand:
        --    pair = (steps, paramsDef)
        -- stepsOpParams_out = concat $ map (\s -> (show s) ++ "\n") stepsOpParams
        (stepsPAR, _) = foldr1 (combineSteps syncChans procIdPAR chansDefPAR) stepsOpParams

        -- create a new ProcId, ProcDef, ProcInst
        procDefPAR = ProcDef chansDefPAR paramsDefPAR (wrapSteps stepsPAR)
        procDefsPAR = Map.insert procIdPAR procDefPAR procDefs''
        procInstPAR = procInst procIdPAR chansInst paramsInsts
    return (procInstPAR, procDefsPAR)
    where
      -- takes two operands (steps and paramsDef) and combines them according to parallel semantics
      combineSteps :: Set.Set ChanId -> ProcId -> [ChanId] -> ([BExpr], [VarId]) -> ([BExpr], [VarId]) ->  ([BExpr], [VarId])
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
        -- TODO: optimize complexity of this: might be MANY steps...
        (stepsL' ++ stepsR' ++ stepsLR, opParamsL ++ opParamsR)
        where
          mergeStepsLR :: ProcId -> [ChanId] -> [VarId] -> [VarId] -> (BExpr, BExpr) -> BExpr
          mergeStepsLR procIdPAR' chansDefPar _opParamsL _opParamsR (stepL, stepR) =
            let -- decompose steps
                ActionPref ActOffer{offers=offersL, constraint=constraintL, hiddenvars=hiddenvarsL} bL = TxsDefs.view stepL
                ProcInst _procIdL _chansL paramsL = TxsDefs.view bL
                ActionPref ActOffer{offers=offersR, constraint=constraintR, hiddenvars=hiddenvarsR} bR = TxsDefs.view stepR
                ProcInst _procIdR _chansR paramsR = TxsDefs.view bR

                -- combine action offers
                --  union of offers, concatenation of constraints
                offersLR = Set.union offersL offersR

                constraintLR = cstrAnd (Set.fromList [constraintL, constraintR])

                -- new ActOffers and ProcInst
                actOfferLR = ActOffer { offers = offersLR,
                                        hiddenvars = Set.union hiddenvarsL hiddenvarsR,
                                        constraint = constraintLR}
                procInstLR = procInst procIdPAR' chansDefPar (paramsL ++ paramsR)
            in
            actionPref actOfferLR procInstLR

          -- check if given step can be executed by itself according to parallel semantics
          isValidStep :: Set.Set ChanId -> BExpr -> Bool
          isValidStep syncChans' (TxsDefs.view -> ActionPref ActOffer{offers=os} _) =
            let -- extract all chanIds from the offers in the ActOffer
                chanIds = Set.foldl (\accu offer -> (chanid offer : accu)) [] os in
            -- if there are no common channels with the synchronisation channels: return true
            Set.null $ Set.intersection syncChans' (Set.fromList chanIds)
          isValidStep _ _ = error "only allowed with ActionPref"

          -- check if given step combination can be executed according to parallel semantics
          isValidStepCombination :: Set.Set ChanId -> (BExpr, BExpr) -> Bool
          isValidStepCombination syncChansSet (TxsDefs.view -> ActionPref ActOffer{offers=offersL} _, TxsDefs.view -> ActionPref ActOffer{offers=offersR} _) =
            let -- extract all chanIds from the offers in the ActOffer
                chanIdsLSet = Set.fromList $ Set.foldl (\accu offer -> (chanid offer : accu)) [] offersL
                chanIdsRSet = Set.fromList $ Set.foldl (\accu offer -> (chanid offer : accu)) [] offersR
                
                intersectionLR = Set.intersection chanIdsLSet chanIdsRSet
                intersectionLsyncChans = Set.intersection chanIdsLSet syncChansSet
                intersectionRsyncChans = Set.intersection chanIdsRSet syncChansSet
            in
            (intersectionLR == intersectionLsyncChans) && (intersectionLsyncChans == intersectionRsyncChans)
          isValidStepCombination _ _ = error "only allowed with tuple of ActionPrefs"

          updateProcInstL :: [VarId] -> ProcId -> [ChanId] -> BExpr -> BExpr
          updateProcInstL opParamsR' procIdPAR' chansDefPar (TxsDefs.view -> ActionPref actOfferL (TxsDefs.view -> ProcInst _procIdInstL _chansInstL paramsInstL)) =
              actionPref actOfferL (procInst procIdPAR' chansDefPar (paramsInstL ++ map cstrVar opParamsR'))
          updateProcInstL _ _ _ _ = error "only allowed with ActionPref >-> ProcInst"
          
          updateProcInstR :: [VarId] -> ProcId -> [ChanId] -> BExpr -> BExpr
          updateProcInstR opParamsL' procIdPAR' chansDefPar (TxsDefs.view -> ActionPref actOfferR (TxsDefs.view -> ProcInst _procIdInstR _chansInstR paramsInstR)) =
              actionPref actOfferR (procInst procIdPAR' chansDefPar (map cstrVar opParamsL' ++ paramsInstR))
          updateProcInstR _ _ _ _ = error "only allowed with ActionPref >-> ProcInst"
          
      -- accu = (opNr, steps, params, procDefs)
      -- foldl : (a -> b -> a) -> a -> [b] -> a
      -- a = (opNr, steps, params, procDefs)
      -- b = BExpr
      -- transform list of bexprs to our big accu combination
      translateOperand :: (EnvB.EnvB envb) => (Int, [([BExpr], [VarId])], [VExpr], ProcDefs ) -> BExpr -> envb (Int, [([BExpr], [VarId])], [VExpr], ProcDefs)
      translateOperand (opNr, stepsOpParams, paramsInsts, procDefs'') operand = do
        -- translate operand to ProcInst if necessary
        (opProcInst, procDefs''') <- transformToProcInst operand procIdInst procDefs''
        -- translate to lpe
        (TxsDefs.view -> ProcInst procIdLPE chansInstLPE paramsInstLPE, procDefs'''') <- lpe opProcInst translatedProcDefs procDefs'''

        let -- decompose translated ProcDef
            ProcDef chansDef paramsDef bexpr = fromMaybe (error "translateOperand: could not find the given procId") (Map.lookup procIdLPE procDefs'''')

            -- instantiate the channels
            chanmap = Map.fromList (zip chansDef chansInstLPE)
            bexpr' = relabel chanmap bexpr
            -- prefix the params and wrap them as VExpr just to be able to use the substitution function later
            prefix = "op" ++ show opNr ++ "$"
            -- TODO: create new unids as well!
        paramsDefPrefixed <- mapM (prefixVarId prefix) paramsDef
        let paramMap = Map.fromList $ zip paramsDef (map cstrVar paramsDefPrefixed)
            -- TODO: properly initialise funcDefs param of subst
            bexpr'' = Subst.subst paramMap (Map.fromList []) bexpr'
        return (opNr+1, stepsOpParams ++ [(extractSteps bexpr'', paramsDefPrefixed)], paramsInsts ++ paramsInstLPE, procDefs'''')
          where 
            transformToProcInst :: (EnvB.EnvB envb) => BExpr -> ProcId -> ProcDefs -> envb(BExpr, ProcDefs)
            -- if operand is already a ProcInst: no need to change anything
            transformToProcInst bexpr@(TxsDefs.view -> ProcInst{}) _procIdParent procDefs''' = return (bexpr, procDefs''')
            -- otherwise: create new ProcDef and ProcInst
            transformToProcInst operand' procIdParent procDefs''' = do
              unid' <- EnvB.newUnid
              let -- decompose parent ProcDef
                  ProcDef chansDef paramsDef _bexpr = fromMaybe (error "transformToProcInst: could not find the given procId") (Map.lookup procIdParent procDefs')
                  -- create new ProcDef and ProcInst
                  procIdNewName = T.pack $ T.unpack (ProcId.name procIdInst) ++ "$op" ++ show opNr
                  procIdNew = procIdInst { ProcId.name = procIdNewName,
                                            ProcId.unid = unid'}
                  procDefNew = ProcDef chansDef paramsDef operand'
                  procDefs'''' = Map.insert procIdNew procDefNew procDefs'''

                  procInst' = procInst procIdNew chansDef (map cstrVar paramsDef)
              return (procInst', procDefs'''')
lpePar _ _ _ = error "only allowed with ProcInst"

prefixVarId :: (EnvB.EnvB envb) => String -> VarId -> envb VarId
prefixVarId prefix (VarId name' _ sort') = do
    unid' <- EnvB.newUnid
    let name'' = T.pack $ prefix ++ T.unpack name'
    return $ VarId name'' unid' sort'


-- ----------------------------------------------------------------------------------------- --
-- LPEHide :
-- ----------------------------------------------------------------------------------------- --

-- we assume that the top-level bexpr of the called ProcDef is HIDE
lpeHide :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
lpeHide procInst'@(TxsDefs.view -> ProcInst procIdInst _chansInst _paramsInst) translatedProcDefs procDefs' = do
    let -- 
        ProcDef chansDef paramsDef bexpr = fromMaybe (error "lpeHide: could not find the given procId") (Map.lookup procIdInst procDefs')
        Hide hiddenChans bexpr' = TxsDefs.view bexpr
        
        -- translate bexpr of HIDE to LPE first
        -- reuse current ProcDef: strip the HIDE operator, just leave the bexpr
        procDef' = ProcDef chansDef paramsDef bexpr'
        procDefs'' = Map.insert procIdInst procDef' procDefs'
    (procInst_lpe@(TxsDefs.view -> ProcInst procId_lpe _chansInst_lpe _paramsInst_lpe), procDefs''') <- lpe procInst' translatedProcDefs procDefs''

    
    let -- decompose translated ProcDef
        ProcDef chansDef_lpe paramsDef_lpe bexpr_lpe = fromMaybe (error "lpeHide: could not find the given procId") (Map.lookup procId_lpe procDefs''')
        -- strip hidden chans and update ProcDef
        steps = extractSteps bexpr_lpe
    steps' <- mapM (hideChans hiddenChans) steps
    let procDef_lpe = ProcDef chansDef_lpe paramsDef_lpe (wrapSteps steps')
        procDefs'''' = Map.insert procId_lpe procDef_lpe procDefs'''
    return (procInst_lpe, procDefs'''')
    where
        hideChans :: EnvB.EnvB envb => Set.Set ChanId -> BExpr -> envb BExpr
        hideChans _ bexpr | isStop bexpr = return stop
        -- hideChans _ (ActionPref chanIdExit ) ??
        hideChans hiddenChans (TxsDefs.view -> ActionPref actOffer bexpr) = do
            let os = offers actOffer 
            (os',hidvars, varMap) <-  hideChansOffer (Set.toList os) 

            -- need to substitute the standard chanoffer variables with the newly unique variables
            --  in constraints and the ProcInst (bepxr)
            let varMap' = Map.fromList $ map (Control.Arrow.second cstrVar) varMap
                constraint_substituted = Subst.subst varMap' (Map.fromList []) $ constraint actOffer
                bexpr_substituted = Subst.subst varMap' (Map.fromList []) bexpr
            return $ actionPref 
                        actOffer{ offers = Set.fromList os'
                                , constraint = constraint_substituted
                                , hiddenvars = hidvars} 
                        bexpr_substituted
            where
                hideChansOffer :: (EnvB.EnvB envb) => [Offer] -> envb([Offer], Set.Set VarId, [(VarId, VarId)])
                hideChansOffer [] = return ([], Set.empty, [])
                hideChansOffer (o:os) = do  -- recursion 
                                            (os', hiddenVarsRec, varMapRec) <- hideChansOffer os 
                                            -- current case
                                            if chanid o `Set.member` hiddenChans 
                                                then do (hidvars, varMap) <- generate_hiddenvars o
                                                        return (os', Set.union hidvars hiddenVarsRec, varMap ++ varMapRec)
                                                else return (o:os', hiddenVarsRec, varMapRec)
                                            where
                                                generate_hiddenvars :: (EnvB.EnvB envb) => Offer -> envb(Set.Set VarId, [(VarId, VarId)])
                                                generate_hiddenvars o'  = do let vars' = extractVarIdsOffer o' []
                                                                             vars'' <- mapM transformVar vars' 
                                                                             let varMap = zip vars' vars''
                                                                             return (Set.fromList vars'', varMap)
                                                -- make hiddenvars globally unique, practically they get a local scope
                                                transformVar :: EnvB.EnvB envb => VarId -> envb VarId
                                                transformVar var' = do  unid' <- EnvB.newUnid
                                                                        let name' = T.unpack (VarId.name var') ++ "_" ++ show unid'
                                                                        return var' { VarId.name = T.pack name', VarId.unid = unid'}
        hideChans _ _ = error "hideChans: unknown input"            

lpeHide _ _ _ = error "lpeHide: was called with something other than a ProcInst"
    



-- ----------------------------------------------------------------------------------------- --
-- preGNFEnable :
-- ----------------------------------------------------------------------------------------- --

-- we assume that the top-level bexpr of the called ProcDef is Enable
preGNFEnable :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
preGNFEnable procInst'@(TxsDefs.view -> ProcInst procIdInst _chansInst _paramsInst) translatedProcDefs procDefs' = do
    let -- 
        ProcDef chansDef paramsDef bexpr = fromMaybe (error "preGNFEnable: could not find the given procId") (Map.lookup procIdInst procDefs')
        Enable bexprL acceptChanOffers bexprR = TxsDefs.view bexpr
        
        -- translate left bexpr of Enable to LPE first
        -- reuse current ProcDef: 
        procDef' = ProcDef chansDef paramsDef bexprL
        procDefs'' = Map.insert procIdInst procDef' procDefs'
    (procInst_lpe@(TxsDefs.view -> ProcInst procId_lpe _chansInst_lpe _paramsInst_lpe), procDefs''') <- lpe procInst' translatedProcDefs procDefs''

    unidR <- EnvB.newUnid
    let -- decompose translated ProcDef
        ProcDef chansDef_lpe paramsDef_lpe bexpr_lpe = fromMaybe (error "preGNFEnable: could not find the given procId") (Map.lookup procId_lpe procDefs''')
        -- strip hidden chans and update ProcDef
        steps = extractSteps bexpr_lpe

        -- create new ProcDef of bexprR
        paramsAccept = extractVarIdsChanOffers acceptChanOffers
        procDefR = ProcDef chansDef (paramsDef ++ paramsAccept) bexprR
        -- create new ProcId
        name' = T.append (ProcId.name procIdInst) (T.pack "$enable")
        procIdR = procIdInst {  ProcId.name = name',
                                ProcId.unid = unidR,
                                -- concatenate the original params with the varIds passed via ACCEPT to extend their scope into the RHS of ENABLE
                                ProcId.procvars = paramsDef ++ paramsAccept} 
        -- create ProcInst, translate params to VExprs
        paramsDef' = map cstrVar paramsDef
        procInstR = procInst procIdR chansDef paramsDef'
        -- put created ProcDefs in the ProcDefs
        procDefs'''' = Map.insert procIdR procDefR procDefs'''

        steps' = map (replaceExits procInstR) steps 

        -- put new steps in the ProcDef of bexprL:
        procDefNew = ProcDef chansDef_lpe paramsDef_lpe (wrapSteps steps')
        procDefs5 = Map.insert procId_lpe procDefNew procDefs''''

    procDefs6 <- preGNF procId_lpe translatedProcDefs procDefs5
      
    return (procInst_lpe, procDefs6)
    where   
        -- just for the steps with EXIT -> <>  replace with the ProcInst
        replaceExits :: BExpr -> BExpr -> BExpr 
        replaceExits (TxsDefs.view -> ProcInst procId chanIds params) bexpr'@(TxsDefs.view -> ActionPref actOffer'@ActOffer{offers = offers'} _bexpr) = --{ offers = Set.fromList [Offer { chanid = chanIdExit, chanoffers = []}]}
            let exitParams = extractVars actOffer' in 
            case Set.toList offers' of 
                [Offer { chanid = chid}] | chid == chanIdExit -> actionPref 
                                                                    actOffer'{  offers = Set.empty
                                                                                , hiddenvars = Set.fromList exitParams} 
                                                                    (procInst procId chanIds (params ++ map cstrVar exitParams))
                _       -> bexpr'
        replaceExits _ _ = error "replaceExits: unknown input"  

preGNFEnable _ _ _ = error "preGNFEnable: was called with something other than a ProcInst"
    


-- ----------------------------------------------------------------------------------------- --
-- LPE :
-- ----------------------------------------------------------------------------------------- --


-- | wrapper around lpe function, returning only the relevant ProcDef instead of all ProcDefs
lpeTransform :: (EnvB.EnvB envb )    --   Monad for unique identifiers and error messages
             => BExpr                -- ^ behaviour expression to be transformed,
                                     --   assumed to be a process instantiation
             -> ProcDefs             -- ^ context of process definitions in which process
                                     --   instantiation is assumed to be defined
             -> envb (Maybe (BExpr, ProcDef))
                                     -- ^ transformed process instantiation with LPE definition
-- template function for lpe
lpeTransform procInst' procDefs'  =
     case TxsDefs.view procInst' of
       ProcInst procid _chans _vexps
         -> case Map.lookup procid procDefs' of
              Just _procdef
                -> lpeTransform' procInst' procDefs'
              _ -> do EnvB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                     "LPE Transformation: undefined process instantiation" ]
                      return Nothing
       _ -> do EnvB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                              "LPE Transformation: only defined for process instantiation" ]
               return Nothing


-- carsten original function for lpe
lpeTransform' :: (EnvB.EnvB envb )   -- Monad for unique identifiers and error messages
             => BExpr                -- ^ behaviour expression to be transformed,
                                     --   assumed to be a process instantiation
             -> ProcDefs             -- ^ context of process definitions in which process
                                     --   instantiation is assumed to be defined
             -> envb (Maybe (BExpr, ProcDef))
lpeTransform' procInst''' procDefs' = do    (procInst', procDefs'') <- lpe procInst''' emptyTranslatedProcDefs procDefs'
                                            procIdInstUid <- EnvB.newUnid
                                            let ProcInst procIdInst'' chansInst paramsInst = TxsDefs.view procInst'
                                                ProcDef chans params bexpr = fromMaybe (error "lpeTransform: could not find the given procId") (Map.lookup procIdInst'' procDefs'')

                                                -- rename ProcId P to LPE_<P>
                                                -- put new ProcId in the procInst
                                                procIdName' = (T.pack $ "LPE_" ++ T.unpack (ProcId.name procIdInst''))
                                                procIdInst' = procIdInst'' { ProcId.name = procIdName',
                                                                            ProcId.unid = procIdInstUid}
                                                procInst'' = procInst procIdInst' chansInst paramsInst

                                                -- put new ProcId in each step
                                                steps = map (substituteProcId procIdInst'' procIdInst') (extractSteps bexpr)
                                                procDef = ProcDef chans params (wrapSteps steps)
                                            return $ Just (procInst'', procDef)
        where
        substituteProcId :: ProcId -> ProcId -> BExpr -> BExpr
        substituteProcId _orig _new bexpr | isStop bexpr = stop
        substituteProcId orig new (TxsDefs.view -> ActionPref actOffer (TxsDefs.view -> ProcInst procId chansInst paramsInst)) =
          if procId == orig
            then actionPref actOffer (procInst new chansInst paramsInst)
            else error "Found a different ProcId, thus the given BExpr is probably not in LPE format"
        substituteProcId orig new  (TxsDefs.view -> ActionPref actOffer 
                                        (TxsDefs.view -> Guard vexpr' 
                                            bexpr'@(TxsDefs.view -> ProcInst procId chansInst paramsInst))) =
            if procId == orig
                then TxsDefs.guard vexpr' $ actionPref actOffer (procInst new chansInst paramsInst)
                else error $ "Found a different ProcId, thus the given BExpr is probably not in LPE format. \n\norig: " ++ show orig  ++
                                            "\nnew: " ++ show new ++
                                            "\nbexpr': " ++ show bexpr' 
        substituteProcId orig new bexpr = error $ "Only allowed with Stop or (ActionPref >-> ProcInst)\n orig: " ++ show orig  ++
                                                            "\nnew: " ++ show new ++
                                                            "\nbexpr: " ++ show bexpr 
        
lpe :: (EnvB.EnvB envb ) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb (BExpr, ProcDefs)
lpe bexprProcInst@(TxsDefs.view -> ProcInst procIdInst _chansInst _paramsInst) translatedProcDefs procDefs' = do
      -- remember the current ProcId to avoid recursive loops translating the same ProcId again
      let translatedProcDefs' = translatedProcDefs { lLPE = lLPE translatedProcDefs ++ [procIdInst]}
      
      -- first translate to GNF
      (procDefsGnf, _gnfTodo) <- gnf procIdInst Map.empty translatedProcDefs' procDefs'

      -- decompose translated ProcDef
      let ProcDef chansDef _paramsDef bexpr = fromMaybe (error "LPE: could not find given procId (should be impossible)") (Map.lookup procIdInst procDefsGnf)

      let accuInit = [(procIdInst, chansDef)]
      let calledProcs = calledProcDefs procDefsGnf accuInit (extractSteps bexpr)

      -- create program counter mapping
      pcUnid <- EnvB.newUnid
      let pcName = "pc$" ++ T.unpack (ProcId.name procIdInst)
      let varIdPC = VarId (T.pack pcName) pcUnid intSort
      let pcMapping = Map.fromList $ zip calledProcs [0..]

      (steps, params, procToParams) <- translateProcs calledProcs varIdPC pcMapping procDefsGnf


      -- create the new ProcId, ProcInst, ProcDef
      procIdUnid <- EnvB.newUnid
      let nameNew = T.unpack (ProcId.name procIdInst)
          paramsNew = varIdPC : params
          procIdNew = procIdInst { ProcId.name = T.pack nameNew
                                   , ProcId.unid = procIdUnid
                                   , ProcId.procchans = chansDef
                                   , ProcId.procvars = paramsNew}

          -- update the ProcInsts in the steps
          steps' = map (stepsUpdateProcInsts calledProcs procToParams pcMapping procIdNew) steps

          procDefLpe = ProcDef chansDef paramsNew (wrapSteps steps')
          procDefs'' = Map.insert procIdNew procDefLpe procDefsGnf
          -- update the ProcInst to the new ProcDef
          procInstLPE = updateProcInst bexprProcInst procIdNew calledProcs

      return (procInstLPE, procDefs'')

    where
        -- recursively collect all (ProcId, Channels)-combinations that are called
        calledProcDefs :: ProcDefs -> [Proc] -> [BExpr] -> [Proc]
        calledProcDefs procDefs'' = foldl processStep
          where
            processStep :: [Proc] -> BExpr -> [Proc]
            -- case bexpr == A >-> P'[]()
            processStep accu (TxsDefs.view -> ActionPref _actOffer (TxsDefs.view -> ProcInst procIdInst' chansInst _paramsInst)) =
              if (procIdInst', chansInst) `elem` accu
                then accu
                else let -- add combination to accu
                         accu' = accu ++ [(procIdInst', chansInst)]
                         -- decompose ProcDef
                         ProcDef chansDef _paramsDef bexprDef = fromMaybe (error "LPE: could not find given procId (should be impossible)") (Map.lookup procIdInst' procDefs'')
                         -- instantiate bexpr with channels of ProcInst
                         chanmap = Map.fromList (zip chansDef chansInst)
                         bexprRelabeled = relabel chanmap bexprDef
                         -- go through steps recursively
                         accu'' = calledProcDefs procDefs'' accu' (extractSteps bexprRelabeled) in
                     accu''

            -- case bexpr == A >-> STOP: nothing to collect
            processStep proc _bexpr = proc

        -- translate all Procs (procId, channels)-combination
        translateProcs :: (EnvB.EnvB envb ) => [Proc] -> VarId -> PCMapping -> ProcDefs -> envb ([BExpr], [VarId], ProcToParams)
        translateProcs [] _ _ _ = return ([], [], Map.fromList [])
        translateProcs (currentProc@(procId, chans):procss) varIdPC pcMapping procDefs'' = do
            let   -- decompose translated ProcDef
                  ProcDef chansDef paramsDef bexprDef = fromMaybe (error $ "\ntranslateProcs: could not find given procId (should be impossible)" ++ show procId ++
                                                                            "\nprocDefs: " ++ show procDefs'') (Map.lookup procId procDefs'')
                  
                  steps = extractSteps bexprDef

                  -- prepare channel instantiation
                  chanMap = Map.fromList (zip chansDef chans)
                  -- prepare param renaming (uniqueness)
                  chanNames = map (\chanId -> "$" ++ T.unpack (ChanId.name chanId) ) chans
                  prefix = T.unpack (ProcId.name procId) ++ concat chanNames ++ "$"

            -- prefix the params and wrap them as VExpr just to be able to use the substitution function later
            prefixedParams' <- mapM (prefixVarId prefix) paramsDef
            let prefixedParams = map cstrVar prefixedParams'
                paramMap = Map.fromList $ zip paramsDef prefixedParams

            let pcValue = fromMaybe (error "translateProcs: could not find the pcValue for given proc (should be impossible)") (Map.lookup currentProc pcMapping)

            -- let steps' = map (lpeBExpr chanMap paramMap varIdPC pcValue) steps
            steps' <- mapM (lpeBExpr chanMap paramMap varIdPC pcValue) steps
            let steps'' = filter (not . isStop) steps'       -- filter out the Stops
            -- recursion
            (stepsRec, paramsRec, procToParamsRec) <- translateProcs procss varIdPC pcMapping procDefs''

            -- add collected prefixed params for later usage
            let params = prefixedParams' ++ paramsRec
            let procToParams = Map.insert currentProc prefixedParams' procToParamsRec
            return (steps'' ++ stepsRec, params, procToParams)

        -- update the original ProcInst, initialise with artifical values
        updateProcInst :: BExpr -> ProcId -> [Proc] -> BExpr
        updateProcInst (TxsDefs.view -> ProcInst _procIdInst chansInst paramsInst) procIdNew _calledProcs =
            let pcValue = cstrConst (Cint 0)
                -- get the params, but leave out the first ones (those of procIdInst itself)
                -- plus an extra one (that of the program counter)
                params = snd $ splitAt (length paramsInst+1) (ProcId.procvars procIdNew)
                paramsSorts = map varIdToSort params
                paramsANYs = map (cstrConst . Cany) paramsSorts
                paramsNew = (pcValue : paramsInst) ++ paramsANYs in
            procInst procIdNew chansInst paramsNew
        updateProcInst _ _ _ = error "Only allowed with ProcInst"
        
        -- update the ProcInsts in the steps to the new ProcId
        stepsUpdateProcInsts :: [Proc] -> ProcToParams -> PCMapping -> ProcId -> BExpr -> BExpr
        stepsUpdateProcInsts _procs _procToParams _pcMap procIdNew (TxsDefs.view -> ActionPref actOffer bexpr) | isStop bexpr =
            let -- get the params, but leave out the first one because it's the program counter
                (_:params) = ProcId.procvars procIdNew
                paramsSorts = map varIdToSort params
                paramsANYs = map (cstrConst . Cany) paramsSorts
                paramsInst = (cstrConst (Cint (-1)) : paramsANYs)
                chansInst = ProcId.procchans procIdNew
                procInst' = procInst procIdNew chansInst paramsInst in
            actionPref actOffer procInst'

        stepsUpdateProcInsts procs procToParams pcMap procIdNew (TxsDefs.view -> ActionPref actOffer procInst''@(TxsDefs.view -> ProcInst procIdInst' chansInst _paramsInst)) =
            let -- collect params AND channels from procs in the order they appear in procs
                paramsNew = createParams procs procInst''

                pcValue = fromMaybe (error "stepsUpdateProcInsts: no pc value found for given (ProcId, [ChanId]) (should be impossible)") (Map.lookup (procIdInst', chansInst) pcMap)

                procInst' = procInst procIdNew (ProcId.procchans procIdNew) ( cstrConst (Cint pcValue) : paramsNew) in
            actionPref actOffer procInst'
            where
                createParams :: [Proc] -> BExpr -> [VExpr]
                createParams [] _ = []
                createParams (proc@(procId, procChans):procs') procInst'''@(TxsDefs.view -> ProcInst procIdInst'' chansInst' paramsInst) =
                    let paramsRec = createParams procs' procInst'''
                        params = if (procId, procChans) == (procIdInst'', chansInst')
                                    then paramsInst
                                    else case Map.lookup proc procToParams of
                                             Just ps   -> map cstrVar ps
                                             Nothing   -> error "createParams: no params found for given proc (should be impossible)"
                    in
                    params ++ paramsRec
                createParams _ _ = error "Only allowed with list of tuples and ProcInst"
                
        stepsUpdateProcInsts _ _ _ _ bexpr = bexpr


        varIdToSort :: VarId -> SortId
        varIdToSort (VarId _ _ sort') = sort'


lpe _ _ _ = error "Only allowed with ProcInst"

lpeBExpr :: (EnvB.EnvB envb ) => ChanMapping -> ParamMapping -> VarId -> Integer -> BExpr -> envb BExpr
lpeBExpr chanMap paramMap varIdPC pcValue (TxsDefs.view -> Guard vexpr' bexpr)  = do
    bexpr' <- lpeBExpr chanMap paramMap varIdPC pcValue bexpr
    return (TxsDefs.guard vexpr' bexpr') 

lpeBExpr _chanMap _paramMap _varIdPC _pcValue bexpr | isStop bexpr = return stop

lpeBExpr chanMap paramMap varIdPC pcValue bexpr = do
    let -- instantiate the bexpr
        bexprRelabeled = relabel chanMap bexpr
        -- TODO: properly initialise funcDefs param of subst
        bexprSubstituted = Subst.subst paramMap (Map.fromList []) bexprRelabeled

        -- decompose bexpr, bexpr' can be STOP or ProcInst (distinction later)
        ActionPref actOffer bexpr' = TxsDefs.view bexprSubstituted


    (offers', constraints', varMap) <- translateOffers (Set.toList (offers actOffer))


    let -- constraints of offer need to be substituted:
        -- say A?x [x>1], this becomes A?A1 [A1 > 1]
        constraintOfOffer = constraint actOffer
        varMap' = Map.fromList $ map (Control.Arrow.second cstrVar) varMap

        -- TODO: properly initialise funcDefs param of subst
        constraintOfOffer' = Subst.subst varMap' (Map.fromList []) constraintOfOffer
        constraintsList = constraintOfOffer' : constraints'
        constraintPC = cstrEqual (cstrVar varIdPC) (cstrConst (Cint pcValue))


        -- if there is a constraint other than just the program counter check
        --    i.e. the normal constraint is empty (just True)
        -- then evaluate the program counter constraint first in an IF clause
        --    to avoid evaluation of possible comparisons with ANY in the following constraint
        constraint' = if constraintsList == [cstrConst (Cbool True)]
                        then constraintPC
                        else cstrITE constraintPC (cstrAnd (Set.fromList constraintsList)) (cstrConst (Cbool False))

        actOffer' = ActOffer { offers = Set.fromList offers'
                             , hiddenvars = hiddenvars actOffer
                             , constraint = constraint' }

        bexpr'' = if isStop bexpr' then stop
                                        -- TODO: properly initialise funcDefs param of subst
                                   else Subst.subst varMap' (Map.fromList []) bexpr'
    return (actionPref actOffer' bexpr'')

    where
        -- transform actions: e.g. A?x [x == 1] becomes A?A1 [A1 == 1]
        translateOffers :: (EnvB.EnvB envb ) => [Offer] -> envb ([Offer], [VExpr], [(VarId, VarId)])
        translateOffers [] = return ([], [], [])
        translateOffers (offer:offers') = do
            let chanId = chanid offer
                chanOffers = chanoffers offer

                chanOffersNumberedSorts = zip [1..] chanOffers

            (chanOffers', constraints, varMap) <- translateChanOffers chanOffersNumberedSorts chanId

            let offer' = offer { chanoffers = chanOffers'}
            (offersRec, constraintsRec, varMapRec) <- translateOffers offers'

            return (offer':offersRec, constraints ++ constraintsRec, varMap ++ varMapRec)
            where
                translateChanOffers :: (EnvB.EnvB envb ) => [(Int, ChanOffer)] -> ChanId -> envb ([ChanOffer], [VExpr], [(VarId, VarId)])
                translateChanOffers [] _ = return ([], [], [])
                translateChanOffers ((i, chanOffer) : chanOffers) chanId = do
                     -- recursion first
                     (chanOffersRec, constraintsRec, varMapRec) <- translateChanOffers chanOffers chanId
                     -- transform current chanOffer
                     let chanName' = T.unpack (ChanId.name chanId) ++ "$" ++ show i
 
                     -- reuse unid of the chanId
                     let varIdChani = VarId (T.pack chanName') (ChanId.unid chanId) (sortOf chanOffer)
                         chanOffer' = Quest varIdChani
                         constraints = case chanOffer of
                                         (Quest _varId)  -> constraintsRec
                                         (Exclam vexpr') -> let constraint' = cstrEqual (cstrVar varIdChani) vexpr' in
                                                                 (constraint':constraintsRec)
                         varMap = case chanOffer of
                                         (Quest varId)   -> (varId, varIdChani) : varMapRec
                                         (Exclam _vexpr)  -> varMapRec
                     return (chanOffer':chanOffersRec, constraints, varMap)
 
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
