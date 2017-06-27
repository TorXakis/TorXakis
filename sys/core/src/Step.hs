{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


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

import Control.Monad.State

import Trace
import CoreUtils

import qualified EnvCore   as IOC

import qualified EnvData   as EnvData
import qualified TxsDDefs  as TxsDDefs
import qualified TxsShow   as TxsShow


-- ----------------------------------------------------------------------------------------- --
-- stepN :  make 'depth' random steps (visible) from current mstate
--       :  steps are input or output, no quiescence, i.e., trace semantics


stepN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
stepN depth step  =  do
     if  depth == 0
       then do
         return $ TxsDDefs.Pass
       else do
         menu  <- traceModelMenu
         mact  <- randMenu menu
         case mact of
         { Nothing  -> do
              IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "probably deadlock" ]
              return $ TxsDDefs.Fail TxsDDefs.ActQui
         ; Just TxsDDefs.ActQui -> do
              IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "no stepping with quiescence" ]
              return $ TxsDDefs.Fail TxsDDefs.ActQui
         ; Just act@(TxsDDefs.Act acts) -> do
              IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                            $ (TxsShow.showN step 6) ++ ": " ++ (TxsShow.fshow act) ]
              done <- traceModelAfter acts
              if  done
                then do modify $ \env -> env
                          { IOC.behtrie  = (IOC.behtrie env) ++
                                           [ ( IOC.curstate env, act, (IOC.maxstate env)+1 ) ]
                          , IOC.curstate = (IOC.maxstate env)+1
                          , IOC.maxstate = (IOC.maxstate env)+1
                          }
                        stepN (depth-1) (step+1)
                else do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                      $ "cannot do selected action" ]
                        return $ TxsDDefs.Fail act
         }


-- ----------------------------------------------------------------------------------------- --
-- stepA :  make step with specified action
--       :  step is input or output, no quiescence, i.e., trace semantics


stepA :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict 
stepA act  =  do
     case act of
     { TxsDDefs.ActQui -> do
          IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "no stepping with quiescence" ]
          return $ TxsDDefs.Fail TxsDDefs.ActQui
     ; act@(TxsDDefs.Act acts) -> do
          IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Step :  " ++ (TxsShow.fshow act) ]
          done <- traceModelAfter acts
          if  done
            then do modify $ \env -> env
                      { IOC.behtrie  = (IOC.behtrie env) ++
                                       [ ( IOC.curstate env, act, (IOC.maxstate env)+1 ) ]
                      , IOC.curstate = (IOC.maxstate env)+1
                      , IOC.maxstate = (IOC.maxstate env)+1
                      }
                    return $ TxsDDefs.Pass
            else do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                  $ "cannot do selected action" ]
                    return $ TxsDDefs.Fail act
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

