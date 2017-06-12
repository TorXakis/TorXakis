{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Trace

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
--  Action Primitives for Trace Semantics on a model, built on Behave
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( traceModelInit      -- :: IOC.IOC ()
, traceModelMenu      -- :: IOC.IOC BTree.Menu
, traceModelAfter     -- :: BTree.BehAction -> IOC.IOC Bool
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.Map  as Map

import CoreUtils

import qualified EnvCore   as IOC
import qualified EnvData   as EnvData
import qualified TxsDefs   as TxsDefs

import qualified Behave    as Behave
import qualified BTree     as BTree


-- ----------------------------------------------------------------------------------------- --
-- traceModelInit :  initialize model for trace semantics


traceModelInit :: IOC.IOC ()
traceModelInit  =  do
     TxsDefs.DefModel (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp) <- gets IOC.modeldef
     allSyncs       <- return $ insyncs ++ outsyncs ++ splsyncs
     envb           <- filterEnvCtoEnvB
     (maybt',envb') <- lift $ runStateT (Behave.behInit allSyncs bexp) envb
     writeEnvBtoEnvC envb'
     modify $ \envc -> envc { IOC.modsts   = case maybt' of
                                             { Nothing  -> Map.empty
                                             ; Just bt' -> Map.singleton 0 bt'
                            }                }


-- ----------------------------------------------------------------------------------------- --
-- traceModelMenu :  menu on current btree of model, no quiescence, according to trace sem


traceModelMenu :: IOC.IOC BTree.Menu
traceModelMenu  =  do     
     TxsDefs.DefModel (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp) <- gets IOC.modeldef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     curState <- gets IOC.curstate
     modSts   <- gets IOC.modsts
     menu     <- case Map.lookup curState modSts of
                 { Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "no curstate" ]
                                 return $ [] 
                 ; Just bt -> do return $ Behave.behMayMenu allSyncs bt
                 } 
     return $ menu


-- ----------------------------------------------------------------------------------------- --
-- traceModelAfter :  do action on current btree and change environment accordingly
--                 :  result gives success, ie. whether act can be done,
--                 :  if not succesful env is not changed
--                 :  act must be a non-empty BehAction


traceModelAfter :: BTree.BehAction -> IOC.IOC Bool
traceModelAfter acts  =  do
     TxsDefs.DefModel (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp) <- gets IOC.modeldef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     curState <- gets IOC.curstate
     nexState <- gets IOC.nexstate
     modSts   <- gets IOC.modsts
     curBTree <- case Map.lookup curState modSts of
                 { Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "no curstate" ]
                                 return $ []
                 ; Just bt -> do return $ bt
                 }
     envb           <- filterEnvCtoEnvB
     (maybt',envb') <- lift $ runStateT (Behave.behAfterAct allSyncs curBTree acts) envb
     writeEnvBtoEnvC envb'
     case maybt' of
     { Nothing  -> do return $ False
     ; Just bt' -> do modify $ \env -> env { IOC.modsts = Map.insert nexState bt' modSts }
                      return $ True
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

