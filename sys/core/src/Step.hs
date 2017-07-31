{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
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

( stepN      -- :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
, stepA      -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict 
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import Control.Monad.State

import qualified Data.Map  as Map

import CoreUtils

import qualified EnvCore   as IOC

import qualified EnvData   as EnvData
import qualified TxsDefs   as TxsDefs
import qualified TxsDDefs  as TxsDDefs
import qualified TxsShow   as TxsShow

import qualified Behave    as Behave


-- ----------------------------------------------------------------------------------------- --
-- stepN :  make 'depth' random steps (visible) from current mstate
--       :  steps are input or output, no quiescence, i.e., trace semantics


stepN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
stepN depth step  =  do
     if  depth == 0
       then do
         return $ TxsDDefs.Pass
       else do
         envc <- get
         case IOC.state envc of
            IOC.Stepping {..} -> do
                let TxsDefs.ModelDef insyncs outsyncs splsyncs bexp = modeldef
                    allSyncs = insyncs ++ outsyncs ++ splsyncs
                    stEnvc = IOC.state envc
                    curState = IOC.curstate stEnvc
                    nexState = IOC.maxstate stEnvc + 1
                    modSts   = case Map.lookup curState (IOC.modstss stEnvc) of
                               { Nothing -> []
                               ; Just bt -> bt
                               }
                    menu     = Behave.behMayMenu allSyncs modSts
                mact <- randMenu menu
                case mact of
                  Nothing  -> do
                       IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "no state or deadlock" ]
                       return $ TxsDDefs.Fail TxsDDefs.ActQui
                  Just TxsDDefs.ActQui -> do
                       IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "no step with quiescence" ]
                       return $ TxsDDefs.NoVerdict
                  Just act@(TxsDDefs.Act acts) -> do
                       IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                     $ (TxsShow.showN step 6) ++ ": " ++ (TxsShow.fshow act) ]
                       envb           <- filterEnvCtoEnvB
                       (maybt',envb') <- lift $ runStateT
                                                  (Behave.behAfterAct allSyncs modSts acts) envb
                       case maybt' of
                       { Nothing  -> do
                              IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "cannot do action" ]
                              return $ TxsDDefs.Fail act
                       ; Just bt' -> do
                              writeEnvBtoEnvC envb'
                              IOC.modifyCS $ \st -> st
                                { IOC.modstss  = Map.insert nexState bt' (IOC.modstss stEnvc) }
                              nextBehTrie act
                              stepN (depth-1) (step+1)
                       }
            _ -> do
                IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Stepping not in Stepper mode" ]
                return $ TxsDDefs.NoVerdict


-- ----------------------------------------------------------------------------------------- --
-- stepA :  make step with specified action
--       :  step is input or output, no quiescence, i.e., trace semantics


stepA :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict 
stepA act  =  do
     envSt <- gets IOC.state
     case (act, envSt) of
     { ( TxsDDefs.ActQui
       , _
       ) -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "no stepping with quiescence" ]
            return $ TxsDDefs.Fail TxsDDefs.ActQui
     ; ( act@(TxsDDefs.Act acts), IOC.Stepping {..} )-> do
            let TxsDefs.ModelDef insyncs outsyncs splsyncs bexp = modeldef
                allSyncs = insyncs ++ outsyncs ++ splsyncs
                curState = IOC.curstate envSt
                nexState = (IOC.maxstate envSt) + 1
                modSts   = case Map.lookup curState (IOC.modstss envSt) of
                           { Nothing -> []
                           ; Just bt -> bt
                           }
            IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                          $ (TxsShow.showN 1 6) ++ ": " ++ (TxsShow.fshow act) ]
            envb           <- filterEnvCtoEnvB
            (maybt',envb') <- lift $ runStateT (Behave.behAfterAct allSyncs modSts acts) envb
            case maybt' of
            { Nothing  -> do
                   IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "cannot do action" ]
                   return $ TxsDDefs.Fail act
            ; Just bt' -> do
                   writeEnvBtoEnvC envb'
                   IOC.modifyCS $ \st -> st
                     { IOC.modstss = Map.insert nexState bt' (IOC.modstss envSt) }
                   nextBehTrie act
                   return $ TxsDDefs.Pass
            }
     ; ( _
       , _
       ) -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Stepping not in Stepper mode" ]
            return $ TxsDDefs.NoVerdict
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

