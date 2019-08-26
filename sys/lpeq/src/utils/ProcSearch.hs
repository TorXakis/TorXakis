{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ProcSearch
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ProcSearch (
showProcId,
getProcsInBExpr,
printProcsInBody,
printProcsInBExpr,
showProcsInBExpr
) where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified EnvData
import qualified TxsDefs
import qualified TxsShow
import qualified SortId
import qualified ProcId
import qualified ProcDef
import qualified ChanId
import qualified VarId
import BehExprDefs
import ProcIdFactory

showProcId :: ProcId.ProcId -> String
showProcId pid = Text.unpack (ProcId.name pid) ++ "[" ++ show (ProcId.unid pid) ++ "]"

-- Lists all processes that can be reached from the given behavioral expression.
-- Works by depth-first-search.
getProcsInBExpr :: TxsDefs.BExpr -> IOC.IOC (Set.Set TxsDefs.ProcId)
getProcsInBExpr = searchBExprForProcs Set.empty

searchBExprForProcs :: Set.Set TxsDefs.ProcId -> TxsDefs.BExpr -> IOC.IOC (Set.Set TxsDefs.ProcId)
searchBExprForProcs soFar currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid _cids _vexprs) ->
             if Set.member pid soFar
             then return soFar
             else do r <- getProcById pid
                     case r of
                       Just (ProcDef.ProcDef _cidDecls _vidDecls body) ->
                           searchBExprForProcs (Set.insert pid soFar) body
                       Nothing -> error ("Unknown process (\"" ++ showProcId pid ++ "\")!")
      (TxsDefs.view -> Guard _g bexpr) ->
          searchBExprForProcs soFar bexpr
      (TxsDefs.view -> Choice bexprs) ->
          Monad.foldM searchBExprForProcs soFar (Set.toList bexprs)
      (TxsDefs.view -> Parallel _cidSet bexprs) ->
          Monad.foldM searchBExprForProcs soFar bexprs
      (TxsDefs.view -> Hide _cidSet bexpr) ->
          searchBExprForProcs soFar bexpr
      (TxsDefs.view -> Enable bexpr1 _acceptOffers bexpr2) ->
          do soFar' <- searchBExprForProcs soFar bexpr1
             searchBExprForProcs soFar' bexpr2
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          do soFar' <- searchBExprForProcs soFar bexpr1
             searchBExprForProcs soFar' bexpr2
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          do soFar' <- searchBExprForProcs soFar bexpr1
             searchBExprForProcs soFar' bexpr2
      (TxsDefs.view -> ActionPref _actOffer bexpr) ->
          searchBExprForProcs soFar bexpr
      (TxsDefs.view -> ValueEnv _venv bexpr) ->
          searchBExprForProcs soFar bexpr
      -- (TxsDefs.view -> StAut _sid _venv transitions) -> 
          -- ...
      _ -> error ("Behavioral expression not accounted for (\"" ++ show currentBExpr ++ "\")!")
-- searchBExprForProcs

printProcsInBody :: String -> ProcId.ProcId -> IOC.IOC ()
printProcsInBody caption pid = do
    r <- getProcById pid
    case r of
      Just (ProcDef.ProcDef _cidDecls _vidDecls body) -> printProcsInBExpr caption body
      Nothing -> error ("Unknown process (\"" ++ showProcId pid ++ "\")!")
-- printProcsInBody

printProcsInBExpr :: String -> TxsDefs.BExpr -> IOC.IOC ()
printProcsInBExpr caption startBExpr = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY caption ]
    strs <- showProcsInBExpr startBExpr
    Monad.mapM_ (\m -> IOC.putMsgs [ EnvData.TXS_CORE_ANY m ]) strs
-- printProcsInBExpr

showProcsInBExpr :: TxsDefs.BExpr -> IOC.IOC [String]
showProcsInBExpr startBExpr = do
    procIds <- getProcsInBExpr startBExpr
    strPerProc <- concat <$> Monad.mapM showProc (Set.toList procIds)
    return (("START ::= " ++ TxsShow.fshow startBExpr) : strPerProc)
  where
    showProc :: ProcId.ProcId -> IOC.IOC [String]
    showProc pid = do
        r <- getProcById pid
        case r of
          Just (ProcDef.ProcDef cidDecls vidDecls body) -> return [ "PROCDEF " ++ showProcSig pid cidDecls vidDecls ++ " ::=", TxsShow.fshow body, "ENDDEF" ]
          Nothing -> return [ "PROCDEF " ++ show pid ++ " ::=", "???", "ENDDEF" ]
    -- doProc
    
    showProcSig :: ProcId.ProcId -> [ChanId.ChanId] -> [VarId.VarId] -> String
    showProcSig pid cidDecls vidDecls =
        let nameStr = TxsShow.fshow pid in
        let cidDeclsStr = "[" ++ List.intercalate "; " (map showChanDecl cidDecls) ++ "]" in
        let vidDeclsStr = "(" ++ List.intercalate "; " (map (\v -> TxsShow.fshow v ++ " :: " ++ Text.unpack (SortId.name (VarId.varsort v))) vidDecls) ++ ")" in
          nameStr ++ " " ++ cidDeclsStr ++ " " ++ vidDeclsStr
    -- showProcSig
    
    showChanDecl :: ChanId.ChanId -> String
    showChanDecl cid =
        let nameStr = TxsShow.fshow cid in
        let sortsStr = List.intercalate " # " (map (Text.unpack . SortId.name) (ChanId.chansorts cid)) in
          if List.null (ChanId.chansorts cid)
          then nameStr
          else nameStr ++ " :: " ++ sortsStr
-- showProcsInBExpr

