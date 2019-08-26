{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ProcIdFactory
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module ProcIdFactory (
createFreshProcIdFromProcId,
createFreshProcIdFromChansAndVars,
createFreshProcIdWithDifferentChans,
createFreshProcIdWithDifferentVars,
getProcById,
getProcsByName,
getMsgOrProcFromName,
registerProc,
unregisterProc
) where

import qualified Control.Monad.State as MonadState
import qualified EnvCore as IOC
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified ProcId
import qualified ChanId
import qualified VarId
import qualified SortId
import qualified SortOf
import qualified TxsDefs
-- import qualified TxsShow

-- Creates a fresh process id based on a given process id:
createFreshProcIdFromProcId :: ProcId.ProcId -> IOC.IOC ProcId.ProcId
createFreshProcIdFromProcId pid = do
    let prefix = Text.unpack (ProcId.name pid)
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    let usedNames = TxsDefs.usedNames tdefs
    let namePrefix = reverse (dropWhile Char.isDigit (reverse prefix))
    let nameSuffix = getNextNameSuffix usedNames namePrefix
    let uniqueName = Text.pack (namePrefix ++ show nameSuffix)
    let tdefs' = tdefs { TxsDefs.usedNames = Map.insert (Text.pack namePrefix) nameSuffix usedNames }
    IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
    varUnid <- IOC.newUnid
    let newPid = (pid { ProcId.name = uniqueName, ProcId.unid = varUnid })
    -- IOC.putInfo [ "New process id (" ++ show usedNames ++ ") = " ++ TxsShow.fshow newPid ++ "[" ++ show varUnid ++ "]" ]
    return newPid
  where
    getNextNameSuffix :: Map.Map Text.Text Int -> String -> Int
    getNextNameSuffix usedNames namePrefix =
        case usedNames Map.!? Text.pack namePrefix of
          Just nameSuffix -> nameSuffix + 1
          Nothing -> 1
    -- getNextNameSuffix
-- getProcIdFromName

createFreshProcIdFromChansAndVars :: Text.Text -> [ChanId.ChanId] -> [VarId.VarId] -> ProcId.ExitSort -> IOC.IOC ProcId.ProcId
createFreshProcIdFromChansAndVars procName cids vids exit = do
    i <- IOC.initUnid
    createFreshProcIdFromProcId (ProcId.ProcId procName i (map ProcId.toChanSort cids) (map SortOf.sortOf vids) exit)
-- createFreshProcIdFromChansAndVars

createFreshProcIdWithDifferentChans :: ProcId.ProcId -> [ChanId.ChanId] -> IOC.IOC ProcId.ProcId
createFreshProcIdWithDifferentChans (ProcId.ProcId name _ _ vids exit) cids = do
    i <- IOC.initUnid
    createFreshProcIdFromProcId (ProcId.ProcId name i (map ProcId.toChanSort cids) vids exit)
-- createFreshProcIdWithDifferentChans

createFreshProcIdWithDifferentVars :: ProcId.ProcId -> [SortId.SortId] -> IOC.IOC ProcId.ProcId
createFreshProcIdWithDifferentVars (ProcId.ProcId name _ cids _ exit) sids = do
    i <- IOC.initUnid
    createFreshProcIdFromProcId (ProcId.ProcId name i cids sids exit)
-- createFreshProcIdWithDifferentVars

-- Gets all processes with a given name:
getProcById :: ProcId.ProcId -> IOC.IOC (Maybe TxsDefs.ProcDef)
getProcById procId = do
    envc <- MonadState.get
    case IOC.state envc of
      IOC.Initing { IOC.tdefs = tdefs } ->
        return (TxsDefs.procDefs tdefs Map.!? procId)
      _ -> return Nothing
-- getProcById

-- Gets all processes with a given name:
getProcsByName :: Text.Text -> IOC.IOC (Map.Map ProcId.ProcId TxsDefs.ProcDef)
getProcsByName procName = do
    envc <- MonadState.get
    case IOC.state envc of
      IOC.Initing { IOC.tdefs = tdefs } ->
        return (Map.filterWithKey (\(TxsDefs.ProcId n _ _ _ _) _ -> n == procName) (TxsDefs.procDefs tdefs))
      _ -> return Map.empty
-- getProcsByName

-- Gets the id and definition of an existing process, or
-- produces a message describing which process names would have been valid:
getMsgOrProcFromName :: Text.Text -> IOC.IOC (Either String (ProcId.ProcId, TxsDefs.ProcDef))
getMsgOrProcFromName procName = do
    envc <- MonadState.get
    case IOC.state envc of
      IOC.Initing { IOC.tdefs = tdefs } ->
        do let procDefs = TxsDefs.procDefs tdefs
           let matchingProcs = Map.filterWithKey (\(TxsDefs.ProcId n _ _ _ _) _ -> n == procName) procDefs
           if matchingProcs == Map.empty
           then return (Left ("Expected " ++ List.intercalate " or " (map (Text.unpack . ProcId.name) (Map.keys procDefs)) ++ ", found " ++ Text.unpack procName ++ "!"))
           else return (Right (head (Map.toList matchingProcs)))
      _ -> return (Left "TorXakis core is not initialized!")
-- getMsgOrProcFromName

registerProc :: TxsDefs.ProcId -> TxsDefs.ProcDef -> IOC.IOC ()
registerProc pid pdef = do
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    let tdefs' = tdefs { TxsDefs.procDefs = Map.insert pid pdef (TxsDefs.procDefs tdefs) }
    IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
-- registerProc

unregisterProc :: TxsDefs.ProcId -> IOC.IOC ()
unregisterProc pid = do
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    let tdefs' = tdefs { TxsDefs.procDefs = Map.delete pid (TxsDefs.procDefs tdefs) }
    IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
-- unregisterProc



