{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  TxsCore
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- API for TorXakis core functionality.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

module TxsCore

(
  -- * run TorXakis core
  runTxsCore

  -- * initialize TorXakis core
, txsInit

  -- * terminate TorXakis core
, txsTermit

  -- * get Mode of TorXakis core
, txsGetMode

  -- * get all TorXakis definitions
, txsGetTDefs

  -- * get all Parameter values
, txsGetParams

  -- * add torxakis definitions
, txsAddTDefs

  -- ** set value of parameter(s)
, txsSetParam

)


-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           System.IO
import           System.Random

-- import from local
import           CoreUtils
import           Ioco
import           Mapper
import           NComp
import           Purpose
import           Sim
import           Step
import           Test

import           Config              (Config)
import qualified Config

-- import from bexpr
import           Relabel             (relabel)

-- import from behave(defs)
import qualified Behave
import qualified BTree

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData
import qualified ParamCore

-- import from defs
import qualified Sigs
import qualified TxsDDefs
import qualified TxsDefs
import qualified TxsShow
import           TxsUtils

-- import from solve
import qualified FreeVar
import qualified SMT
import qualified Solve
import qualified Solve.Params
import qualified SolveDefs
import qualified SMTData

-- import from value
import qualified Eval

-- import from lpe
-- import qualified LPE
import qualified LPE

-- import from valexpr
import qualified SortId
import qualified SortOf
import Constant
import VarId


-- ----------------------------------------------------------------------------------------- --
-- run TorXakis core


-- | TorXakis core main api -- start
runTxsCore :: Config -> StateT s IOC.IOC a -> s -> IO ()
runTxsCore initConfig ctrl s0  =  do
      _ <- runStateT (runTxsCtrl ctrl s0)
              IOC.EnvC { IOC.config = initConfig
                       , IOC.unid   = 0
                       , IOC.params = Config.updateParamVals -- updating parameters...
                                        initParams -- ...defined in EnvCore and SolveDefs
                                        $ Config.configuredParameters initConfig
                       , IOC.state  = initState
                       }
      return ()
      where initState  = IOC.Idling
            initParams = Map.union ParamCore.initParams Solve.Params.initParams

runTxsCtrl :: StateT s IOC.IOC a -> s -> IOC.IOC ()
runTxsCtrl ctrl s0  =  do
     _ <- runStateT ctrl s0
     return ()


-- ----------------------------------------------------------------------------------------- --
-- initializing and terminating


-- | Initialize TorXakis core
--
--   Only possible when in Idling Mode.
txsInit :: TxsDefs.TxsDefs                 -- ^ Definitions for computations.
        -> Sigs.Sigs VarId                 -- ^ Signatures needed to parse.
        -> ([EnvData.Msg] -> IOC.IOC ())   -- ^ Handler for info, warning, and error messages.
        -> IOC.IOC (Either Error ())
txsInit tdefs sigs putMsgs  =  do
     envc <- get
     case IOC.state envc of
       IOC.Idling 
         -> do
               let cfg    = IOC.config envc
                   smtLog = Config.smtLog cfg
                   -- An error will be thrown if the selected solver is not in
                   -- the list of available solvers. The sanity of the
                   -- configuration is checked outside this function, however
                   -- nothing prevents a client of this function from injecting
                   -- a wrong configuration. A nicer error handling requires
                   -- some refactoring of the TorXakis core to take this into
                   -- account.
                   smtProc = fromJust (Config.getProc cfg)
               smtEnv         <- lift $ SMT.createSMTEnv smtProc smtLog
               (info,smtEnv') <- lift $ runStateT SMT.openSolver smtEnv
               (_,smtEnv'')   <- lift $ runStateT (SMT.addDefinitions (SMTData.EnvDefs (TxsDefs.sortDefs tdefs) (TxsDefs.cstrDefs tdefs) (Set.foldr Map.delete (TxsDefs.funcDefs tdefs) (allENDECfuncs tdefs)))) smtEnv'
               putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Solver " ++ show (Config.solverId (Config.selectedSolver cfg)) ++ " initialized : " ++ info
                       , EnvData.TXS_CORE_USER_INFO   "TxsCore initialized"
                       ]
               put envc {
                 IOC.state =
                     IOC.Initing { IOC.smts    = Map.singleton "current" smtEnv''
                                 , IOC.tdefs   = tdefs
                                 , IOC.sigs    = sigs
                                 , IOC.putmsgs = putMsgs
                                 , IOC.chanoffers = Map.fromList []
                                 }
                 }
               return $ Right ()
       _ -> do return $ Left $ Error "Initialization must be started from Idling Mode"


-- | Terminate TorXakis core
--
--   Only possible when in Initing Mode.
txsTermit :: IOC.IOC (Either Error ())
txsTermit  =  do
     envc <- get
     case IOC.state envc of
       IOC.Initing { IOC.smts    = smts
                   , IOC.putmsgs = putmsgs
                   }
         -> do lift $ mapM_ (runStateT SMT.close) (Map.elems smts)
               putmsgs [ EnvData.TXS_CORE_USER_INFO "Solver(s) closed"
                       , EnvData.TXS_CORE_USER_INFO "TxsCore terminated"
                       ]
               put envc { IOC.state = IOC.Idling }
               return $ Right ()
       _ -> do return $ Left $ Error "Termination must be started from Initing Mode"


-- ----------------------------------------------------------------------------------------- --
-- getters


-- | Get TorXakis Mode
--
--   Possible in all Modes.
txsGetMode :: IOC.IOC (Either Error TxsMode)
txsGetMode  =  do
     envc <- get
     case IOC.state envc of
       Idling    {} ->  return $ Right Idling
       Initing   {} ->  return $ Right Initing
       TestSet   {} ->  return $ Right TestSet
       Testing   {} ->  return $ Right Testing
       SimSet    {} ->  return $ Right SimSet
       Simuling  {} ->  return $ Right Simuling
       StepSet   {} ->  return $ Right StepSet
       Stepping  {} ->  return $ Right Stepping
       ManSet    {} ->  return $ Right ManSet
       Manualing {} ->  return $ Right Manualing 
     

-- | Get all TorXakis definitions
--
--   Possible in all Modes.
txsGetTDefs :: IOC.IOC (Either Error TxsDefs.TxsDefs)
txsGetTDefs  =  do
     envc <- get
     case IOC.state envc of
       _ -> return $ Right $ IOC.tdefs (IOC.state envc)


-- | Get the values of named parameters, or all parameters if empty.
--
--   Possible in any Mode.
txsGetParams :: [String] -> IOC.IOC (Either Error [(String,String)])
txsGetParams prms  =  do
     envc <- get
     case IOC.state envc of
       _ -> do let params' = map (\(nm,(val,_))->(nm,val)) . Map.toList (IOC.params envc)
               case prms of
                 [] -> return $ Right params'
                 _  -> return $ Right $ filter ((`elem` prms) . fst) params'


-- ----------------------------------------------------------------------------------------- --
-- setters


-- | Add TorXakis definitions
--
--   Only possible when in Initing or some X-Set Mode.
--   Added definitions are assumed to be correct and consistent with current definitions.
txsAddTDefs :: TxsDefs.TxsDefs -> IOC.IOC (Either Error ())
txsAddTDefs tdefs'  =  do
     envc <- get
     case IOC.state envc of
       Initing {} ->  Right <$> updateTDefs
       TestSet {} ->  Right <$> updateTDefs
       SimSet  {} ->  Right <$> updateTDefs
       StepSet {} ->  Right <$> updateTDefs
       ManSet  {} ->  Right <$> updateTDefs
       _          ->  return $ Left $ Error
                        "Adding TorXakis Definitions only in Initing or some X-Set Mode"
  where
     tdefs :: TxsDefs.TxsDefs
     tdefs  =  IOC.tdefs envc
     updateTDefs :: IOC.IOC ()
     updateTDefs  =  IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs ++ tdefs' }


-- | Set the provided parameter(s) to the provided value.
--
--   Only possible when in Idling, Initing, or some X-Set Mode.
txsSetParams :: [(String,String)]          -- ^ list of parameter (name,value).
             -> IOC.IOC (Either Error ())
txsSetParams paramvals  =  do
     envc <- get
     case IOC.state envc of
       Idling  {} ->  Right <$> setParams
       Initing {} ->  Right <$> setParams
       TestSet {} ->  Right <$> setParams
       SimSet  {} ->  Right <$> setParams
       StepSet {} ->  Right <$> setParams
       ManSet  {} ->  Right <$> setParams
       _          ->  return $ Left $ Error
                        "Setting parameters only in Idling, Initing, or some X-Set Mode"
  where
     setParams :: IOC.IOC ()
     setParams  =  do
          let (seeds,paramvals') = partition ((==)"param_Seed" . fst) paramvals
          _ <- IOC.setParams paramvals'
          case seeds of
            []                                -> return $ Right ()
            (seed:_) | and (map isDigit seed) -> do lift $ setStdGen(mkStdGen (read seed)::Int)
                                                    IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_INFO
                                                                  "Seed set to " ++ seed ]
                                                    return $ Right ()


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

