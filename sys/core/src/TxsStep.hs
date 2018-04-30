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
  txsSetStep       -- :: D.ModelDef -> IOC.IOC ()

  -- * shut stepping mode
, txsShutStep      -- :: IOC.IOC ()

  -- * start stepping mode
, txsStartStep     -- :: IOC.IOC ()

  -- * stop stepping mode
, txsStopStep      -- :: IOC.IOC ()

  -- * do an action in the model
, txsStepAct       -- :: DD.Action -> IOC.IOC DD.Verdict

  -- * do an action according to offer-pattern in the model
, txsStepOffer     -- :: D.Offer -> IOC.IOC DD.Verdict

  -- * run n actions on the model; if n<0 then indefinitely many actions
, txsStepRun       -- :: Int -> IOC.IOC DD.Verdict

  -- * give the menu of possible offers in the model
, txsStepMenu      -- :: IOC.IOC BTree.Menu

  -- * give current state number in the model
, txsStepStateNr   -- :: IOC.IOC EnvData.StateNr

  -- * give current state in the model
, txsStepState     -- :: IOC.IOC BTree.BTree 

  -- * give trace from initial state to current state in the model
, txsStepTrace     -- :: [DD.Action]

  -- * give transition graph of actions stepped through so far
, txsStepGraph     -- :: [(EnvData.StateNr,DD.Action,EnvData.StateNr)]

  -- * go to the specified state number in the model
, txsStepTo        -- :: EnvData.StateNr -> IOC.IOC ()

  -- * go to the initial state in the model
, txsStepInit      --  :: IOC.IOC ()

  -- * go back with the specified number of steps in the model.
, txsStepBack      --  :: Int -> IOC>IOC ()
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
import qualified EnvData
-- import qualified ParamCore

-- import from defs
-- import qualified Sigs
import qualified TxsDDefs            as DD
import qualified TxsDefs             as D
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
txsSetStep :: D.ModelDef                -- ^ model definition.
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
                          "Stepping Mode must be set from Initing mode" ]

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
 

startStepper :: D.ModelDef -> IOC.IOC ( Maybe BTree.BTree )

startStepper (D.ModelDef minsyncs moutsyncs msplsyncs mbexp)  =  do
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
                    , IOC.behtrie  = _behtrie
                    , IOC.inistate = _inistate
                    , IOC.curstate = _curstate
                    , IOC.maxstate = _maxststae
                    , IOC.modstss  = _modstss
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
-- | Step model with the provided action.
--
--   Only possible when Stepping.
txsStepAct :: DD.Action                           -- ^ Action to step in model.
           -> IOC.IOC DD.Verdict                  -- ^ Verdict of stepping.
txsStepAct act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> Step.stepA act
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Stepping with action only in Stepping Mode" ]
               return DD.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- | Step with action according to offer-pattern in the model.
--
--   Only possible when in Stepping Mode.
txsStepOffer :: D.Offer -> IOC.IOC DD.Verdict
txsStepOffer offer  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> do mact <- randOff2Act offer
               case mact of
                 Nothing
                   -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                       "Could not generate action for stepping" ]
                         return DD.NoVerdict
                 Just act
                   -> txsStepAct act
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Stepping with offer only in Stepping Mode" ]
               return DD.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- | Step model with the provided number of actions.
--
--   Only possible when Stepping.
txsStepRun :: Int                                 -- ^ Number of actions to step model.
           -> IOC.IOC DD.Verdict                  -- ^ Verdict of stepping.
txsStepRun nrsteps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> do Step.stepN nrsteps 1
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Stepping with run only in Stepping Mode" ]
               return DD.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- | Give the menu, i.e., all possible offers.
--
--   Only possible when Stepping.
txsStepMenu :: IOC.IOC BTree.Menu
txsStepMenu  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> do menuIn  <- Step.stepModelMenuIn
               menuOut <- Step.stepModelMenuOut
               return $ menuIn ++ menuOut
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Stepping menu only in Stepping Mode" ]
               return []

-- ----------------------------------------------------------------------------------------- --
-- | Give current state number
--
--   Only possible when Stepping.
txsStepStateNr :: IOC.IOC EnvData.StateNr
txsStepStateNr  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.curstate = curstate }
         -> return curstate
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Current state of stepping only in Stepping Mode" ]
               return $ -1

-- ----------------------------------------------------------------------------------------- --
-- | Give current state 
--
--   Only possible when Stepping.
txsStepState :: IOC.IOC BTree.BTree
txsStepState  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.curstate = curstate
                    , IOC.modstss  = modstss
                    }
         -> do case Map.lookup curstate modstss of
                 Nothing    -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                "No valid current state" ]
                                  return []
                 Just btree -> return btree
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Current state of stepping only in Stepping Mode" ]
               return []
                
-- ----------------------------------------------------------------------------------------- --
-- | Give trace from initial state to current state
--
--   Only possible when Stepping.
txsStepTrace :: IOC.IOC [DD.Action]
txsStepTrace  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.behtrie  = behtrie
                    , IOC.inistate = inistate
                    , IOC.curstate = curstate
                    }
         -> case trace behtrie inistate curstate of
              Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                          "Path error: Behaviour Trie is not a tree" ]
                            return []
              Just t  -> return t
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Trace of stepping only in Stepping Mode" ]
               return []
  where
     trace :: [(EnvData.StateNr, DD.Action, EnvData.StateNr)]
           -> EnvData.StateNr
           -> EnvData.StateNr
           -> (Maybe [DD.Action])
     trace _behtrie from to    | from >  to  =  Nothing
     trace _behtrie from to    | from == to  =  Just []
     trace  behtrie from to -- | from <  to
       =  case [ (s1,a,s2) | (s1,a,s2) <- behtrie, s2 == to ] of
            [(s1,a,_s2)] -> case trace behtrie from s1 of
                             Nothing -> Nothing
                             Just t  -> Just $ t ++ [a]
            _           -> Nothing

-- ----------------------------------------------------------------------------------------- --
-- | Give the transition graph of actions stepped through so far.
--
--   Only possible when Stepping.
txsStepGraph :: IOC.IOC [(EnvData.StateNr, DD.Action, EnvData.StateNr)]
txsStepGraph  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.behtrie  = behtrie }
         -> return behtrie
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Graph of stepping only in Stepping Mode" ]
               return []

-- ----------------------------------------------------------------------------------------- --
-- | Go to state with the provided state number in the model
--
--   Only possible when Stepping.
txsStepTo :: EnvData.StateNr               -- ^ state to go to.
          -> IOC.IOC ()
txsStepTo statenr  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.modstss = modstss }
         -> case Map.lookup statenr modstss of
              Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                       "No such state to go to" ]
              Just _  -> IOC.modifyCS $ \st -> st { IOC.curstate = statenr }
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Go to stepping state only in Stepping Mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Go to initial in the model
--
--   Only possible when Stepping.
txsStepInit :: IOC.IOC ()
txsStepInit  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.inistate = inistate
                    , IOC.modstss  = modstss
                    }
         -> case Map.lookup inistate modstss of
              Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                       "No initial state to go to" ]
              Just _  -> IOC.modifyCS $ \st -> st { IOC.curstate = inistate }
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Go to initial stepping state only in Stepping Mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Go back with the specified number of steps in the model.
--
--   Only possible when Stepping.
txsStepBack :: Int -> IOC.IOC ()
txsStepBack steps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.behtrie  = behtrie
                    , IOC.curstate = curstate
                    , IOC.modstss  = modstss
                    }
         -> case Map.lookup curstate modstss of
              Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                       "No initial state to go backwards from" ]
              Just _  -> let newstate = back behtrie curstate steps
                          in case Map.lookup newstate modstss of
                           Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                    "No state to go backwards to" ]
                           Just _  -> IOC.modifyCS $ \st -> st { IOC.curstate = newstate }
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Go backwards in stepping state only in Stepping Mode" ]
  where
     back :: [(EnvData.StateNr, DD.Action, EnvData.StateNr)]
          -> EnvData.StateNr
          -> Int
          -> EnvData.StateNr
     back _behtrie cur stps    | stps <= 0  =  cur
     back  behtrie cur stps -- | stps >  0
       =  case [ (s1,a,s2) | (s1,a,s2) <- behtrie, s2 == cur ] of
            [(s1,_a,_s2)] -> back behtrie s1 (stps-1)
            _             -> (-1)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

