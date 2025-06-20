{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TXS2MCRL2
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns #-}
module LPE2MCRL2 (
lpe2mcrl2
) where

import Control.Monad.State hiding (guard, state)
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified FreeMonoidX as FMX
import qualified EnvCore as IOC
import qualified EnvData
import qualified TxsDefs
import qualified SortId
import qualified SortDef
import qualified FuncId
import qualified FuncDef
import qualified VarId
import qualified ValExpr
import qualified ChanId
import qualified CstrId
import qualified CstrDef
import qualified Constant

import qualified MCRL2Defs
import MCRL2PrettyPrint
import MCRL2Env
import LPETypes
import ValFactory
import LPEChanMap

-- import TxsShow

lpe2mcrl2 :: LPEOperation
lpe2mcrl2 lpe out invariant = do
    let initialEnv = emptyEnv { txsdefs = lpeContext lpe }
    evalStateT (lpeProcess2mcrl2 lpe out invariant) initialEnv
-- lpe2mcrl2

lpeProcess2mcrl2 :: LPE -> String -> TxsDefs.VExpr -> T2MMonad (Either [String] LPE)
lpeProcess2mcrl2 lpe out _invariant = do
    lift $ IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<mcrl2>>" ]
    tdefs <- gets txsdefs
    let orderedParams = Map.keys (lpeInitEqs lpe)
    -- Translate sorts.
    -- (These are just identifiers; they are defined further via constructors.)
    sorts <- Monad.mapM sortDef2sortDef (Map.toList (TxsDefs.sortDefs tdefs))
    modifySpec (\spec -> spec { MCRL2Defs.sorts = Map.fromList sorts })
    -- Translate constructors:
    Monad.mapM_ constructor2constructor (Map.toList (TxsDefs.cstrDefs tdefs))
    -- Translate function headers to mappings:
    mappings <- Monad.mapM function2mapping (Map.toList (TxsDefs.funcDefs tdefs))
    modifySpec (\spec -> spec { MCRL2Defs.mappings = Map.fromList mappings })
    -- Translate function bodies to equation groups:
    eqGroups <- Monad.mapM function2eqGroup (Map.toList (TxsDefs.funcDefs tdefs))
    modifySpec (\spec -> spec { MCRL2Defs.equationGroups = eqGroups })
    -- Translate channels:
    -- actions <- Monad.mapM createFreshAction (Set.insert TxsDefs.chanIdIstep (Set.toList (lpeChanParams lpe)))
    actions <- Monad.mapM createFreshAction (Set.toList (revertSimplChanIdsWithChanMap (lpeChanMap lpe) (lpeChanParams lpe)))
    modifySpec (\spec -> spec { MCRL2Defs.actions = Map.fromList actions })
    -- Translate LPE header:
    (lpeProcName, lpeProc) <- createLPEProcess orderedParams
    modifySpec (\spec -> spec { MCRL2Defs.processes = Map.fromList [(lpeProcName, lpeProc)] })
    -- Translate LPE body:
    newSummands <- Monad.mapM (summand2summand lpe (lpeProcName, lpeProc) orderedParams) (Set.toList (lpeSummands lpe))
    let newProcess = lpeProc { MCRL2Defs.expr = MCRL2Defs.PChoice newSummands }
    modifySpec (\spec -> spec { MCRL2Defs.processes = Map.insert lpeProcName newProcess (MCRL2Defs.processes spec) })
    -- Translate LPE initialization:
    lpeInit <- procInst2procInst (lpeProcName, lpeProc) orderedParams (lpeInitEqs lpe)
    modifySpec (\spec -> spec { MCRL2Defs.init = lpeInit })
    spec <- gets specification
    let filename = out ++ ".mcrl2"
    liftIO $ writeFile filename (showSpecification spec)
    return (Left ["Model exported to " ++ filename ++ "!"])
-- lpe2mcrl2'

-- Creates an mCRL2 sort declaration from a TXS sort declaration:
sortDef2sortDef :: (SortId.SortId, SortDef.SortDef) -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Sort)
sortDef2sortDef (sortId, _) = do
    sortName <- getFreshName (SortId.name sortId)
    registerObject (TxsDefs.IdSort sortId) (RegSort sortName)
    if sortId == SortId.sortIdBool
    then return (sortName, MCRL2Defs.BoolSort)
    else if sortId == SortId.sortIdInt
         then return (sortName, MCRL2Defs.IntSort)
         else if (sortId == SortId.sortIdString) || (sortId == SortId.sortIdRegex)
              then return (sortName, MCRL2Defs.ListSort MCRL2Defs.IntSort)
              -- Non-primitive SortDefs do not contain information (they are defined via constructors).
              else return (sortName, MCRL2Defs.ImplicitSort) 
-- sortDef2sortDef

-- Creates an mCRL2 constructor from a TXS constructor:
constructor2constructor :: (CstrId.CstrId, CstrDef.CstrDef) -> T2MMonad ()
constructor2constructor (cstrId, CstrDef.CstrDef recognizer projections) = do
    -- Create the mCRL2 constructor:
    constructorName <- getFreshName (CstrId.name cstrId)
    recognizerName <- getFreshName (FuncId.name recognizer)
    projectionNames <- Monad.mapM (getFreshName . FuncId.name) projections
    projectionSorts <- Monad.mapM sort2sort (CstrId.cstrargs cstrId)
    let fields = zipWith (\x y -> MCRL2Defs.Variable { MCRL2Defs.varName = x, MCRL2Defs.varSort = y }) projectionNames projectionSorts
    let newConstructor = MCRL2Defs.Constructor { MCRL2Defs.cstrName = constructorName, MCRL2Defs.fields = fields, MCRL2Defs.recognizer = recognizerName }
    -- Look up the (already created) mCRL2 sort that corresponds with the TXS sort of the constructor:
    (sortName, sort) <- getRegisteredSort (CstrId.cstrsort cstrId)
    registerObject (TxsDefs.IdCstr cstrId) (RegCstr sortName constructorName)
    -- Replace the previous declaration of the mCRL2 sort with one with the new constructor:
    let newStructSort = case sort of
                          MCRL2Defs.StructSort constructors -> MCRL2Defs.StructSort (constructors ++ [newConstructor])
                          _ -> MCRL2Defs.StructSort [newConstructor]
    modifySpec (\spec -> spec { MCRL2Defs.sorts = Map.insert sortName newStructSort (MCRL2Defs.sorts spec) })
-- constructor2constructor

-- Creates an mCRL2 mapping from a TXS function definition.
-- The body of the function is not being translated here, because it may reference objects that do not yet exist!
function2mapping :: (FuncId.FuncId, FuncDef.FuncDef VarId.VarId) -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Sort)
function2mapping (funcId, FuncDef.FuncDef params _expr) = do
    mappingName <- getFreshName (FuncId.name funcId)
    mappingParams <- Monad.mapM createFreshVar params
    mappingResult <- sort2sort (FuncId.funcsort funcId)
    -- Add a mapping for the function:
    let mappingSort = case mappingParams of
                        [] -> mappingResult
                        _ -> MCRL2Defs.FunctionSort (sorts2multiSort (map MCRL2Defs.varSort mappingParams)) mappingResult
    registerObject (TxsDefs.IdFunc funcId) (RegMapping mappingName)
    return (mappingName, mappingSort)
-- function2mapping

-- Converts a list of mCRL2 sorts into a (binary) tree of multi-sorts:
sorts2multiSort :: [MCRL2Defs.Sort] -> MCRL2Defs.Sort
sorts2multiSort [] = MCRL2Defs.ImplicitSort
sorts2multiSort [x] = x
sorts2multiSort xs = MCRL2Defs.MultiSort xs
-- sorts2multiSort

-- Creates an mCRL2 equation group from a TXS function definition:
function2eqGroup :: (FuncId.FuncId, FuncDef.FuncDef VarId.VarId) -> T2MMonad MCRL2Defs.EquationGroup
function2eqGroup (funcId, FuncDef.FuncDef params expr) = do
    (mappingName, _) <- getRegisteredMapping funcId
    -- Construct the equation:
    mappingParams <- Monad.mapM createFreshVar params
    let mappingRef = MCRL2Defs.DMappingRef mappingName (map MCRL2Defs.DVariableRef mappingParams)
    funcExpr <- valExpr2dataExpr expr -- Here, all functions must already have a mapping, because they could be referenced!
    let eqn = MCRL2Defs.Equation { MCRL2Defs.lhs = mappingRef, MCRL2Defs.rhs = funcExpr }
    return $ MCRL2Defs.EquationGroup { MCRL2Defs.variables = mappingParams, MCRL2Defs.equations = [eqn] }
-- function2eqGroup

-- Creates a uniquely named action from a TXS channel definition:
createFreshAction :: TxsDefs.ChanId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Action)
createFreshAction chanId = do
    actionName <- getFreshName (ChanId.name chanId)
    --lift $ IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Renamed " ++ Text.unpack (ChanId.name chanId) ++ " to " ++ Text.unpack actionName) ]
    actionSorts <- Monad.mapM sort2sort (ChanId.chansorts chanId)
    registerObject (TxsDefs.IdChan chanId) (RegAction actionName)
    return (actionName, MCRL2Defs.Action (sorts2multiSort actionSorts))
-- createFreshAction

-- Creates an mCRL2 process from a TXS process.
-- The body of the process is not being translated here, because it may reference objects that do not yet exist!
createLPEProcess :: [VarId.VarId] -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Process)
createLPEProcess paramIds = do
    procName <- getFreshName (Text.pack "LPE")
    procParams <- Monad.mapM createFreshVar paramIds
    return (procName, MCRL2Defs.Process { MCRL2Defs.processParams = procParams, MCRL2Defs.expr = MCRL2Defs.PDeadlock })
-- createLPEProcess

summand2summand :: LPE -> (MCRL2Defs.ObjectId, MCRL2Defs.Process) -> [VarId.VarId] -> LPESummand -> T2MMonad MCRL2Defs.PExpr
summand2summand lpe (lpeProcName, lpeProc) orderedParams summand = do
    -- Create the channel variables (both explicit and hidden) first, so that they can be referenced:
    actionVars <- Monad.mapM createFreshVar (lpeSmdVars summand)
    -- Create actions (with their arguments):
    newActionExpr <- do actionExpr <- channelOffer2actionExpr lpe summand
                        return (MCRL2Defs.PAction actionExpr)
    -- Translate guard and recursive instantiation:
    newGuardExpr <- valExpr2dataExpr (lpeSmdGuard summand)
    newProcInst <- procInst2procInst (lpeProcName, lpeProc) orderedParams (lpeSmdEqs summand)
    -- Combine the different parts:
    return (MCRL2Defs.PSum actionVars (MCRL2Defs.PGuard newGuardExpr (MCRL2Defs.PSeq [newActionExpr, newProcInst]) MCRL2Defs.PDeadlock))
-- summand2summand

procInst2procInst :: (MCRL2Defs.ObjectId, MCRL2Defs.Process) -> [VarId.VarId] -> LPEParamEqs -> T2MMonad MCRL2Defs.PExpr
procInst2procInst (lpeProcName, lpeProc) orderedParams paramEqs = do
    paramValues <- Monad.mapM valExpr2dataExpr (paramEqsLookup orderedParams paramEqs)
    return (MCRL2Defs.PInst lpeProcName (zip (MCRL2Defs.processParams lpeProc) paramValues))
-- paramEqs2procInst

-- channelOffer2action :: LPEChanOffer -> T2MMonad MCRL2Defs.AInstance
channelOffer2actionExpr :: LPE -> LPESummand -> T2MMonad MCRL2Defs.AExpr
channelOffer2actionExpr lpe summand = do
    -- Actions have been remapped for convenience, undo this first:
    let (varsPerChan, _hiddenVars) = getActOfferDataFromChanMap (lpeChanMap lpe) (lpeSmdChan summand) (lpeSmdVars summand)
    -- NOTE: Hidden variables are introduced in the sum operator, which does not exist in TorXakis.
    --       Consequently, we don't have to attach them as a parameter to an action (as happens in TorXakis).
    --       It is (still) assumed that the hidden variables are finitely bounded!
    if null varsPerChan
    then return MCRL2Defs.ATau
    else do translatedAInsts <- Monad.mapM varPerChanToAInstance varsPerChan
            return (MCRL2Defs.AExpr translatedAInsts)
  where
    varPerChanToAInstance :: (ChanId.ChanId, [VarId.VarId]) -> T2MMonad MCRL2Defs.AInstance
    varPerChanToAInstance (cid, vids) = do
        -- The action (after look-up) should already exist:
        (actionName, _action) <- getRegisteredAction cid
        translatedVars <- Monad.mapM getRegisteredVar vids
        let translatedVarExprs = map (\(_varName, varObj) -> MCRL2Defs.DVariableRef varObj) translatedVars
        return (MCRL2Defs.AInstance actionName translatedVarExprs)
-- offer2action

-- Translates a TXS sort to an mCRL2 sort:
sort2sort :: SortId.SortId -> T2MMonad MCRL2Defs.Sort
sort2sort sortId = do
    (sortName, _) <- getRegisteredSort sortId
    return $ MCRL2Defs.SortRef sortName
-- sort2sort

-- Creates a uniquely named mCRL2 variable from a TXS variable:
createFreshVar :: VarId.VarId -> T2MMonad MCRL2Defs.Variable
createFreshVar varId = do
    newVarName <- getFreshName (VarId.name varId)
    --lift $ IOC.putMsgs [ EnvData.TXS_CORE_ANY ("(var) Renamed " ++ Text.unpack (VarId.name varId) ++ " to " ++ Text.unpack newVarName) ]
    newVarSort <- sort2sort (VarId.varsort varId)
    let newVar = MCRL2Defs.Variable { MCRL2Defs.varName = newVarName, MCRL2Defs.varSort = newVarSort }
    registerObject (TxsDefs.IdVar varId) (RegVar newVar)
    return newVar
-- createFreshVar

-- Wrapper around valExpr2dataExpr' so that it is easier to debug:
valExpr2dataExpr :: ValExpr.ValExpr VarId.VarId -> T2MMonad MCRL2Defs.DExpr
valExpr2dataExpr =
    -- liftIO $ putStrLn ((showValExpr expr) ++ " <<|-----|>> " ++ (show expr))
    valExpr2dataExpr' valExpr2dataExpr
-- valExpr2dataExpr

-- Translates a TXS value expression to an mCRL2 data expression:
valExpr2dataExpr' :: (ValExpr.ValExpr VarId.VarId -> T2MMonad MCRL2Defs.DExpr) -> ValExpr.ValExpr VarId.VarId -> T2MMonad MCRL2Defs.DExpr
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vconst (Constant.Cbool value)) =
    return $ MCRL2Defs.DBool value
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vconst (Constant.Cint value)) =
    return $ MCRL2Defs.DInt value
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vconst (Constant.Cstring string)) =
    return $ MCRL2Defs.DList (map (MCRL2Defs.DInt . toInteger . Char.ord) (Text.unpack string))
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vconst (Constant.Cregex _value)) =
    return $ MCRL2Defs.DList [] -- WARNING! Regular expressions are considered to be out of scope!
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vconst (Constant.Ccstr cstrId fieldValues)) = do
    (cstrName, MCRL2Defs.Constructor { MCRL2Defs.fields = fields }) <- getRegisteredCstr cstrId
    translatedFieldValues <- Monad.mapM (f . ValExpr.cstrConst) fieldValues
    return $ MCRL2Defs.DConstructorRef cstrName (zip fields translatedFieldValues)
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vconst (Constant.Cany sortId)) = do
    newGlobalName <- getFreshName (Text.pack ("g" ++ Text.unpack (SortId.name sortId)))
    newGlobalSort <- sort2sort sortId
    let newGlobal = MCRL2Defs.Variable { MCRL2Defs.varName = newGlobalName, MCRL2Defs.varSort = newGlobalSort }
    -- Note that the global does not have to be registered, since it will only be referenced once.
    -- It must be declared in mCRL2 though:
    modifySpec (\spec -> spec { MCRL2Defs.globals = Map.insert newGlobalName newGlobal (MCRL2Defs.globals spec) })
    return $ MCRL2Defs.DVariableRef newGlobal
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vvar var) = do
    (_varName, translatedVar) <- getRegisteredVar var
    return $ MCRL2Defs.DVariableRef translatedVar
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vequal lhs rhs) = do
    translatedLhs <- f lhs
    translatedRhs <- f rhs
    return $ MCRL2Defs.DEqual translatedLhs translatedRhs
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vite condition ifBranch elseBranch) = do
    translatedCondition <- f condition
    translatedIfBranch <- f ifBranch
    translatedElseBranch <- f elseBranch
    return $ MCRL2Defs.DIfThenElse translatedCondition translatedIfBranch translatedElseBranch
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vnot expr) = do
    translatedExpr <- f expr
    return $ MCRL2Defs.DNot translatedExpr
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vand conjuncts) = do
    translatedConjuncts <- Monad.mapM f (Set.toList conjuncts)
    case translatedConjuncts of
      x:xs -> return $ foldr MCRL2Defs.DAnd x xs
      _ -> return $ MCRL2Defs.DBool True -- Should not happen!
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vdivide dividend divisor) = do
    translatedDividend <- f dividend
    translatedDivisor <- f divisor
    return $ MCRL2Defs.DDivide translatedDividend translatedDivisor
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vmodulo dividend divisor) = do
    translatedDividend <- f dividend
    translatedDivisor <- f divisor
    return $ MCRL2Defs.DModulo translatedDividend translatedDivisor
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vsum freeSum) = do
    translatedFreeSum <- Monad.mapM cOccur2dataExpr (FMX.toDistinctAscOccurListT freeSum)
    case translatedFreeSum of
      x:xs -> return $ foldr MCRL2Defs.DAdd x xs
      [] -> return $ MCRL2Defs.DInt 0 -- Should not happen!
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vproduct freeProduct) = do
    translatedFreeProduct <- Monad.mapM cOccur2dataExpr (FMX.toDistinctAscOccurListT freeProduct)
    case translatedFreeProduct of
      x:xs -> return $ foldr MCRL2Defs.DMultiply x xs
      [] -> return $ MCRL2Defs.DInt 1 -- Should not happen!
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vgez expr) = do
    translatedExpr <- f expr
    return $ MCRL2Defs.DGreaterEquals translatedExpr (MCRL2Defs.DInt 0)
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vlength string) = do
    translatedString <- f string
    return $ MCRL2Defs.DListSize translatedString
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vat string index) = do
    translatedString <- f string
    translatedIndex <- f index
    return $ MCRL2Defs.DListElement translatedString translatedIndex
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vconcat strings) = do
    translatedStrings <- Monad.mapM f strings
    case translatedStrings of
      x:xs -> return $ foldr MCRL2Defs.DListConcatenate x xs
      _ -> return $ MCRL2Defs.DList [] -- Should not happen!
valExpr2dataExpr' _f (ValExpr.view -> ValExpr.Vstrinre _string _regex) =
    return $ MCRL2Defs.DBool True -- WARNING! Regular expressions are considered to be out of scope!
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vcstr cstrId fieldValues) = do
    (cstrName, MCRL2Defs.Constructor { MCRL2Defs.fields = fields }) <- getRegisteredCstr cstrId
    translatedFieldValues <- Monad.mapM f fieldValues
    return $ MCRL2Defs.DConstructorRef cstrName (zip fields translatedFieldValues)
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Viscstr cstrId expr) = do
    (_cstrName, MCRL2Defs.Constructor { MCRL2Defs.recognizer = recognizerName }) <- getRegisteredCstr cstrId
    translatedExpr <- f expr
    return $ MCRL2Defs.DRecognizer recognizerName translatedExpr
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vaccess cstrId _accName fieldIndex expr) = do
    (_cstrName, MCRL2Defs.Constructor { MCRL2Defs.fields = fields }) <- getRegisteredCstr cstrId
    translatedExpr <- f expr
    return $ MCRL2Defs.DFieldAccess (MCRL2Defs.varName (fields !! fieldIndex)) translatedExpr
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vfunc funcId paramValues) = do
    (mappingName, _mappingSort) <- getRegisteredMapping funcId
    translatedParamValues <- Monad.mapM f paramValues
    return $ MCRL2Defs.DMappingRef mappingName translatedParamValues
valExpr2dataExpr' f (ValExpr.view -> ValExpr.Vpredef _predefKind funcId _paramValues) = do -- TODO what is Vpredef exactly?
    tdefs <- gets txsdefs
    f (sort2defaultValue tdefs (FuncId.funcsort funcId))
-- valExpr2dataExpr'

-- Used exclusively to translate FreeSums and FreeProducts (FreeMonoidXs):
cOccur2dataExpr :: (ValExpr.ValExpr VarId.VarId, Integer) -> T2MMonad MCRL2Defs.DExpr
cOccur2dataExpr (expr, count) = do
    translatedExpr <- valExpr2dataExpr expr
    return $ MCRL2Defs.DMultiply translatedExpr (MCRL2Defs.DInt count)
-- cOccur2dataExpr




