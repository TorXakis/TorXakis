{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module Ioco

-- ----------------------------------------------------------------------------------------- --
--
-- Test Primitives for an IOCO Model, built on Behave
--
-- ----------------------------------------------------------------------------------------- --
-- export


( iocoModelMenuIn    -- :: IOC.IOC TreeVars.Menu
, iocoModelMenuOut   -- :: IOC.IOC TreeVars.Menu
, iocoModelIsQui     -- :: IOC.IOC Bool
, iocoModelAfter     -- :: TxsDDefs.Action -> IOC.IOC Bool
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import qualified Data.Set            as Set

-- import from local
import           CoreUtils

-- import from behavedef
import qualified STree
import qualified TreeVars

-- import from behaveenv
import qualified SBehave

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData

-- import from defs
import qualified TxsDDefs
import qualified TxsDefs

import qualified Utils


-- ----------------------------------------------------------------------------------------- --
-- iocoModelMenuIn  :  input  menu on current btree of model, no quiescence, according to ioco
-- iocoModelMenuOut :  output menu on current btree of model, no quiescence, according to ioco


iocoModelMenu :: IOC.IOC TreeVars.Menu
iocoModelMenu  =  do
     validModel <- validModDef
     case validModel of
       Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelMenu without valid model" ]
            return []
       Just (TxsDefs.ModelDef insyncs outsyncs splsyncs _bexp) -> do
            let allSyncs = insyncs ++ outsyncs ++ splsyncs
            modSts   <- gets (head . IOC.modsts . IOC.state)
            return $ SBehave.behMayMenu allSyncs modSts

iocoModelMenuIn :: IOC.IOC TreeVars.Menu
iocoModelMenuIn  =  do
     menu <- iocoModelMenu
     filterM (isInCTOffers . Utils.frst) menu


iocoModelMenuOut :: IOC.IOC TreeVars.Menu
iocoModelMenuOut  =  do
     menu <- iocoModelMenu
     filterM (isOutCTOffers . Utils.frst) menu

-- | iocoModelIsQui :  quiescence test on current btree of model
iocoModelIsQui :: IOC.IOC Bool
iocoModelIsQui  =  do
     validModel <- validModDef
     case validModel of
       Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelIsQui without valid model" ]
            return False
       --Just (TxsDefs.ModelDef _insyncs outsyncs _splsyncs _bexp) -> do
            --modSts <- gets (IOC.modsts . IOC.state)
            --return $ SBehave.behRefusal modSts (Set.unions outsyncs)
       _ ->
            error "iocoModelIsQui: not implemented yet!"

-- | iocoModelAfter : do action on current btree and change environment
-- accordingly result gives success, ie. whether act can be done, if not
-- succesful env is not changed act must be a non-empty action; Act ActIn
-- ActOut or ActQui
iocoModelAfter :: TxsDDefs.Action -> IOC.IOC Bool
iocoModelAfter (TxsDDefs.Act acts)  =  do
--iocoModelAfter (TxsDDefs.Act _)  =  do
    validModel <- validModDef
    case validModel of
        Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelAfter without valid model" ]
            return False
        Just (TxsDefs.ModelDef insyncs outsyncs splsyncs _) -> do
            let allSyncs       = insyncs ++ outsyncs ++ splsyncs
            path <- gets $ IOC.modsts . IOC.state
            let curStates = head path
            mayst' <- SBehave.behAfterAct allSyncs curStates acts
            --writeEnvBtoEnvC envb'
            case mayst' of
                Nothing  -> return False
                Just st' -> do
                    IOC.modifyCS $ \st -> st { IOC.modsts = st' : path }
                    return True

{-iocoModelAfter TxsDDefs.ActQui  =  do
    validModel <- validModDef
    case validModel of
        Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelAfter without valid model" ]
            return False
        Just (TxsDefs.ModelDef _ outsyncs _ _) -> do
            modSts         <- gets (head . IOC.modsts . IOC.state)
            path <- gets $ IOC.modsts . IOC.state
            let curStates = head path
            mayst' <- SBehave.behAfterRef curStates (Set.unions outsyncs)
            case mayst' of
                Nothing  -> return False
                Just st' -> do
                    IOC.modifyCS $ \st -> st { IOC.modsts = st' : path}
                    return True
-}
-- ----------------------------------------------------------------------------------------- --

validModDef :: IOC.IOC (Maybe TxsDefs.ModelDef)
validModDef  =  do
     envc <- gets IOC.state
     case envc of
       IOC.Testing {IOC.modeldef = moddef}  -> return $ Just moddef
       IOC.Simuling {IOC.modeldef = moddef} -> return $ Just moddef
       _                                    -> return Nothing

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
