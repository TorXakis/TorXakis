{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE RecordWildCards #-}

-- ----------------------------------------------------------------------------------------- --

module Step

-- ----------------------------------------------------------------------------------------- --
-- 
-- Visibly stepping through an STS
--
-- ----------------------------------------------------------------------------------------- --
-- export

( stepN            -- :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
, stepA            -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict 
, stepModelMenuIn  -- :: IOC.IOC BTree.Menu
, stepModelMenuOut -- :: IOC.IOC BTree.Menu
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State
import qualified Data.Map  as Map
import Data.Maybe

import CoreUtils
import qualified EnvCore   as IOC
import qualified EnvData
import qualified TxsDefs
import qualified TxsDDefs
import qualified TxsShow
import qualified Behave
import qualified BTree
import qualified Utils

-- ----------------------------------------------------------------------------------------- --
-- stepN :  make 'depth' random steps (visible) from current mstate
--       :  steps are input or output, no quiescence, i.e., trace semantics

stepN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
stepN depth step =
     if  depth == 0
       then return TxsDDefs.Pass
       else do
         envc <- get
         case IOC.state envc of
            IOC.Stepping {..} -> do
                let TxsDefs.ModelDef insyncs outsyncs splsyncs _ = modeldef
                    allSyncs = insyncs ++ outsyncs ++ splsyncs
                    stEnvc = IOC.state envc
                    curState = IOC.curstate stEnvc
                    nexState = IOC.maxstate stEnvc + 1
                    modSts   = fromMaybe [] (Map.lookup curState (IOC.modstss stEnvc))
                    menu     = Behave.behMayMenu allSyncs modSts
                mact <- randMenu menu
                case mact of
                  Nothing  -> do
                       IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "no state or deadlock" ]
                       return $ TxsDDefs.Fail TxsDDefs.ActQui
                  Just TxsDDefs.ActQui -> do
                       IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "no step with quiescence" ]
                       return TxsDDefs.NoVerdict
                  Just act@(TxsDDefs.Act acts) -> do
                       IOC.putMsgs [ EnvData.AnAction act ]
                       envb           <- filterEnvCtoEnvB
                       (maybt',envb') <- lift $ runStateT (Behave.behAfterAct allSyncs modSts acts) envb
                       writeEnvBtoEnvC envb'
                       case maybt' of
                         Nothing  -> do
                              IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "stepN - cannot do action" ]
                              return $ TxsDDefs.Fail act
                         Just bt' -> do
                              IOC.modifyCS $ \st -> st
                                { IOC.modstss  = Map.insert nexState bt' (IOC.modstss stEnvc) }
                              nextBehTrie act
                              stepN (depth-1) (step+1)
                        
            _ -> do
                IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Stepping not in Stepper mode" ]
                return TxsDDefs.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- stepA :  make step with specified action
--       :  step is input or output, no quiescence, i.e., trace semantics

stepA :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
stepA act = do
     envSt <- gets IOC.state
     case (act, envSt) of
       ( TxsDDefs.ActQui, _) -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "no stepping with quiescence" ]
            return $ TxsDDefs.Fail TxsDDefs.ActQui
       ( act'@(TxsDDefs.Act acts), IOC.Stepping {..} )-> do
            let TxsDefs.ModelDef insyncs outsyncs splsyncs _ = modeldef
                allSyncs = insyncs ++ outsyncs ++ splsyncs
                curState = IOC.curstate envSt
                nexState = IOC.maxstate envSt + 1
                modSts   = fromMaybe [] (Map.lookup curState (IOC.modstss envSt))
            IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                          $ TxsShow.showN 1 6 ++ ": " ++ TxsShow.fshow act' ]
            envb           <- filterEnvCtoEnvB
            (maybt',envb') <- lift $ runStateT (Behave.behAfterAct allSyncs modSts acts) envb
            writeEnvBtoEnvC envb'
            case maybt' of
              Nothing  -> do
                   IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "stepA - cannot do action" ]
                   return $ TxsDDefs.Fail act'
              Just bt' -> do
                   IOC.modifyCS $ \st -> st
                     { IOC.modstss = Map.insert nexState bt' (IOC.modstss envSt) }
                   nextBehTrie act'
                   return TxsDDefs.Pass
       ( _, _) -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Stepping not in Stepper mode" ]
            return TxsDDefs.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- stepMenu

stepModelMenu :: IOC.IOC BTree.Menu
stepModelMenu  =  do
     envSt <- gets IOC.state
     case envSt of
       IOC.Stepping {IOC.modeldef = TxsDefs.ModelDef insyncs outsyncs splsyncs _bexp} -> do
         let allSyncs = insyncs ++ outsyncs ++ splsyncs
             curState = IOC.curstate envSt
             modSts   = fromMaybe [] (Map.lookup curState (IOC.modstss envSt))
         return $ Behave.behMayMenu allSyncs modSts
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "stepModelMenu without valid model"     ]
         return []

stepModelMenuIn :: IOC.IOC BTree.Menu
stepModelMenuIn  =  do
     menu <- stepModelMenu
     filterM (isInCTOffers . Utils.frst) menu

stepModelMenuOut :: IOC.IOC BTree.Menu
stepModelMenuOut  =  do
     menu <- stepModelMenu
     filterM (isOutCTOffers . Utils.frst) menu


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
