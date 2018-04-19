{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- |
-- Module      :  TxsCore
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- Stepping Mode
-- ----------------------------------------------------------------------------------------- --

-- {-# LANGUAGE OverloadedStrings #-}

module TxsStep

(
  -- * set stepping mode
  txsSetStep       -- :: TxsDefs.ModelDef -> IOC.IOC ()

  -- * shut stepping mode
, txsShutStep      -- :: IOC.IOC ()

  -- * start stepping mode
, txsStartStep     -- :: IOC.IOC ()

  -- * stop stepping mode
, txsStopStep      -- :: IOC.IOC ()

  -- * do an action in the model
, txsStepAct       -- :: TxsDDefs.Action -> IOC.IOC (Either String TxsDDefs.Verdict)

  -- * do an action according to offer-pattern in the model
, txsStepOffer     -- ::

  -- * run n actions on the model; if n<0 then indefinitely many actions
, txsStepRun       -- ::

  -- * give current state number in the model
, txsStepStateNr   -- ::  

  -- * give current state in the model
, txsStepState     -- ::  

  -- * give trace from initial state to current state in the model
, txsStepTrace     -- ::

  -- * give transition graph of actions stepped through so far
, txsStepGraph     -- ::

  -- * give the menu of possible offers in the model
, txsStepMenu      -- :: 

  -- * go to the specified state number in the model
, txsStepTo        -- ::

  -- * go to the initial state in the model
, txsStepInit      --  ::

  -- * go back with the specified number of steps in the model.
, txsStepBack      --  ::
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import           Control.Arrow
-- import           Control.Monad
import           Control.Monad.State
-- import qualified Data.List           as List
import qualified Data.Map            as Map
-- import           Data.Maybe
-- import           Data.Monoid
-- import qualified Data.Set            as Set
-- import qualified Data.Text           as T
-- import           System.Random

-- import from local
import           CoreUtils
-- import           Ioco
import           Step

-- import           Config              (Config)
-- import qualified Config

-- import from behave(defs)
import qualified Behave
import qualified BTree
-- import           Expand              (relabel)

-- import from coreenv
import qualified EnvCore             as IOC
-- import qualified EnvData
-- import qualified ParamCore

-- import from defs
-- import qualified Sigs
import qualified TxsDDefs
import qualified TxsDefs
-- import qualified TxsShow
-- import           TxsUtils

-- import from solve
-- import qualified FreeVar
-- import qualified SMT
-- import qualified Solve
-- -- import qualified SolveDefs
-- import qualified SolveDefs.Params
-- import qualified SMTData

-- import from value
-- import qualified Eval

-- import from valexpr
-- import qualified SortId
-- import qualified SortOf
-- import ConstDefs
-- import VarId


-- ----------------------------------------------------------------------------------------- --
-- | Set Stepping Mode.
--
--   Only possible when txscore is initialized.

txsSetStep :: TxsDefs.ModelDef              -- ^ model definition.
           -> IOC.IOC ()
txsSetStep moddef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Initing { IOC.smts     = smts
                   , IOC.tdefs    = tdefs
                   , IOC.sigs     = sigs
                   , IOC.putmsgs  = putmsgs
                   }
         -> do IOC.putCS IOC.StepSet { IOC.smts     = smts
                                     , IOC.tdefs    = tdefs
                                     , IOC.sigs     = sigs
                                     , IOC.modeldef = moddef
                                     , IOC.putmsgs  = putmsgs
                                     }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepping Mode set" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Stepping Mode must be set in Initing mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Shut Stepping Mode.
--
--   Only possible when in StepSet Mode.
txsShutStep :: IOC.IOC ()
txsShutStep  =  do
     envc <- get
     case IOC.state envc of
       IOC.StepSet { IOC.smts     = smts
                   , IOC.tdefs    = tdefs
                   , IOC.sigs     = sigs
                   , IOC.modeldef = _moddef
                   , IOC.putmsgs  = putmsgs
                   }
         -> do IOC.putCS IOC.Initing { IOC.smts     = smts
                                     , IOC.tdefs    = tdefs
                                     , IOC.sigs     = sigs
                                     , IOC.putmsgs  = putmsgs
                                     }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepping Mode shut" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Stepping Mode must be shut from StepSet Mode" ]
                 
-- ----------------------------------------------------------------------------------------- --
-- | Start stepping.
--
--   Only possible when in StepSet Mode.
txsStartStep :: IOC.IOC ()
txsStartStep  =  do
     envc <- get
     case IOC.state envc of
       IOC.StepSet { IOC.smts     = smts
                   , IOC.tdefs    = tdefs
                   , IOC.sigs     = sigs
                   , IOC.modeldef = moddef
                   , IOC.putmsgs  = putmsgs
                   }
         -> do IOC.putCS IOC.Stepping { IOC.smts      = smts
                                      , IOC.tdefs     = tdefs
                                      , IOC.sigs      = sigs
                                      , IOC.modeldef  = moddef
                                      , IOC.behtrie   = []
                                      , IOC.inistate  = 0
                                      , IOC.curstate  = 0
                                      , IOC.maxstate  = 0
                                      , IOC.modstss   = Map.empty
                                      , IOC.putmsgs   = putmsgs
                                      }
               maybt <- startStepper moddef
               case maybt of
                 Nothing
                   -> IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_INFO
                                    "Starting Stepping Mode failed"]
                 Just bt
                   -> do IOC.modifyCS $ \st -> st { IOC.modstss = Map.singleton 0 bt }
                         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepping Mode started" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Stepping Mode must be started in StepSet Mode" ]
 

startStepper :: TxsDefs.ModelDef -> IOC.IOC ( Maybe BTree.BTree )

startStepper (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)  =  do
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     envb            <- filterEnvCtoEnvB
     (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
     writeEnvBtoEnvC envb'
     return maybt'

-- ----------------------------------------------------------------------------------------- --
-- | Stop stepping.
--
--   Only possible when Stepping Mode.
txsStopStep :: IOC.IOC ()
txsStopStep  =  do
     envc <- get
     case  IOC.state envc of
       IOC.Stepping { IOC.smts     = smts
                    , IOC.tdefs    = tdefs
                    , IOC.sigs     = sigs
                    , IOC.modeldef = moddef
                    , IOC.putmsgs  = putmsgs
                    }
         -> do IOC.putCS IOC.StepSet { IOC.smts     = smts
                                     , IOC.tdefs    = tdefs
                                     , IOC.sigs     = sigs
                                     , IOC.modeldef = moddef
                                     , IOC.putmsgs  = putmsgs
                                     }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepping Mode stopped" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Stepping Mode must be stopped from Stepping Mode" ]

-- ----------------------------------------------------------------------------------------- --
-- txsStepRun

-- | Step model with the provided number of actions.
--
--   Only possible when Stepping.
txsStepRun :: Int                                        -- ^ Number of actions to step model.
           -> IOC.IOC (Either String TxsDDefs.Verdict)   -- ^ Verdict of stepping.
txsStepRun nrsteps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> do verd <- Step.stepN nrsteps 1
               return $ Right verd
       _ -> return $ Left "'txsStepRun' only allowed in 'Stepping' core state"


-- ----------------------------------------------------------------------------------------- --
-- | Step model with the provided action.
--
--   Only possible when Stepping.
txsStepAct :: TxsDDefs.Action                            -- ^ Action to step in model.
           -> IOC.IOC (Either String TxsDDefs.Verdict)   -- ^ Verdict of stepping.
txsStepAct act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> do verd <- Step.stepA act
               return $ Right verd
       _ -> return $ Left "'txsStepAct' only allowed in 'Stepping' core state"


-- ----------------------------------------------------------------------------------------- --

{-

-- | Go to state with the provided state number.
-- core action.
--
-- Only possible in stepper modus (see 'txsSetStep').
txsGoTo :: EnvData.StateNr              -- ^ state to go to.
        -> IOC.IOC ()
txsGoTo stateNr  =
  if  stateNr >= 0
  then do
    modStss <- gets (IOC.modstss . IOC.state)
    case Map.lookup stateNr modStss of
       Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "no such state" ]
       Just _ ->
         modify $
           \env ->
             env { IOC.state =
                     (IOC.state env)
                     { IOC.curstate = stateNr }
                 }
  else ltsBackN (-stateNr)
  where
     ltsBackN :: Int -> IOC.IOC ()
     ltsBackN backsteps
        | backsteps <= 0 = return ()
        | otherwise  = do    -- backsteps > 0
            st <- gets IOC.state
            let iniState = IOC.inistate st
                curState = IOC.curstate st
                behTrie = IOC.behtrie st
            case [ s | (s, _, s') <- behTrie, s' == curState ] of
              [prev] -> do
                modify $
                  \env ->
                    env { IOC.state =
                            (IOC.state env) {
                            IOC.curstate = prev
                            }
                        }
                unless (prev == iniState) (ltsBackN (backsteps-1))
              _      -> do
                IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "LtsBack error" ]
                return ()

-- | Provide the path.
txsPath :: IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
txsPath  =  do
  st <- gets IOC.state
  path (IOC.inistate st) (IOC.curstate st)
  where
     path :: EnvData.StateNr -> EnvData.StateNr ->
             IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
     path from to | from >= to = return []
     path from to = do -- from < to
       iniState <- gets (IOC.inistate . IOC.state)
       behTrie  <- gets (IOC.behtrie . IOC.state)
       case [ (s1,a,s2) | (s1,a,s2) <- behTrie, s2 == to ] of
         [(s1,a,s2)] ->
           if (s1 == from) || (s1 == iniState)
           then return [(s1,a,s2)]
           else do
             pp <- path from s1
             return $ pp ++ [(s1,a,s2)]
         _           -> do
           IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Path error" ]
           return []


-- | Return the menu, i.e., all possible actions.
txsMenu :: String                               -- ^ kind (valid values are "mod", "purp", or "map")
        -> String                               -- ^ what (valid values are "all", "in", "out", or a <goal name>)
        -> IOC.IOC BTree.Menu
txsMenu kind what  =  do
     envSt <- gets IOC.state
     case (kind,envSt) of
       ("mod",IOC.Testing {})  -> do
            menuIn   <- Ioco.iocoModelMenuIn
            menuOut  <- Ioco.iocoModelMenuOut
            case what of
              "all" -> return $ menuIn ++ menuOut
              "in"  -> return menuIn
              "out" -> return menuOut
              _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                          return []
       ("mod",IOC.Simuling {}) -> do
            menuIn   <- Ioco.iocoModelMenuIn
            menuOut  <- Ioco.iocoModelMenuOut
            case what of
              "all" -> return $ menuIn ++ menuOut
              "in"  -> return menuIn
              "out" -> return menuOut
              _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                          return []
       ("mod",IOC.Stepping {}) -> do
            menuIn  <- Step.stepModelMenuIn
            menuOut <- Step.stepModelMenuOut
            case what of
              "all" -> return $ menuIn ++ menuOut
              "in"  -> return menuIn
              "out" -> return menuOut
              _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                          return []
       ("map",IOC.Testing {})  -> Mapper.mapperMenu
       ("map",IOC.Simuling {}) -> Mapper.mapperMenu
       ("purp",IOC.Testing {}) -> Purpose.goalMenu what
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
               return []

-}

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

