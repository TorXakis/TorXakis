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


( iocoModelMenuIn    -- :: IOC.IOC BTree.Menu
, iocoModelMenuOut   -- :: IOC.IOC BTree.Menu
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
import qualified BTree

-- import from behaveenv
import qualified Behave

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


iocoModelMenu :: IOC.IOC BTree.Menu
iocoModelMenu  =  do
     validModel <- validModDef
     case validModel of
       Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelMenu without valid model" ]
            return []
       Just (TxsDefs.ModelDef insyncs outsyncs splsyncs _bexp) -> do
            let allSyncs = insyncs ++ outsyncs ++ splsyncs
            modSts   <- gets (IOC.modsts . IOC.state)
            return $ Behave.behMayMenu allSyncs modSts

iocoModelMenuIn :: IOC.IOC BTree.Menu
iocoModelMenuIn  =  do
     menu <- iocoModelMenu
     filterM (isInCTOffers . Utils.frst) menu


iocoModelMenuOut :: IOC.IOC BTree.Menu
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
       Just (TxsDefs.ModelDef _insyncs outsyncs _splsyncs _bexp) -> do
            modSts <- gets (IOC.modsts . IOC.state)
            return $ Behave.behRefusal modSts (Set.unions outsyncs)

-- | iocoModelAfter : do action on current btree and change environment
-- accordingly result gives success, ie. whether act can be done, if not
-- succesful env is not changed act must be a non-empty action; Act ActIn
-- ActOut or ActQui
iocoModelAfter :: TxsDDefs.Action -> IOC.IOC Bool
iocoModelAfter (TxsDDefs.Act acts)  =  do
     validModel <- validModDef
     case validModel of
       Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelAfter without valid model" ]
            return False
       Just (TxsDefs.ModelDef insyncs outsyncs splsyncs _) -> do
            let allSyncs       = insyncs ++ outsyncs ++ splsyncs
            modSts         <- gets (IOC.modsts . IOC.state)
            envb           <- filterEnvCtoEnvB
            (maybt',envb') <- lift $ runStateT (Behave.behAfterAct allSyncs modSts acts) envb
            writeEnvBtoEnvC envb'
            case maybt' of
              Nothing  -> return False
              Just bt' -> do IOC.modifyCS $ \st -> st { IOC.modsts = bt' }
                             return True

iocoModelAfter TxsDDefs.ActQui  =  do
     validModel <- validModDef
     case validModel of
       Nothing -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "iocoModelAfter without valid model" ]
            return False
       Just (TxsDefs.ModelDef _ outsyncs _ _) -> do
            modSts         <- gets (IOC.modsts . IOC.state)
            envb           <- filterEnvCtoEnvB
            (maybt', envb') <- lift $ runStateT (Behave.behAfterRef modSts (Set.unions outsyncs))
                                                envb
            writeEnvBtoEnvC envb'
            case maybt' of
              Nothing  -> return False
              Just bt' -> do IOC.modifyCS $ \st -> st { IOC.modsts = bt' }
                             return True

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
