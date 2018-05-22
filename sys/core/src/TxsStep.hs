{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- |
-- Module      :  TxsStep
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
  txsSetStep       -- :: D.ModelDef -> IOC.IOC (Either EnvData.Msg ())

  -- * shut stepping mode
, txsShutStep      -- :: IOC.IOC (Either EnvData.Msg ())

  -- * start stepping mode
, txsStartStep     -- :: IOC.IOC (Either EnvData.Msg ())

  -- * stop stepping mode
, txsStopStep      -- :: IOC.IOC (Either EnvData.Msg ())

  -- * do an action in the model
, txsStepAct       -- :: DD.Action -> IOC.IOC (Either EnvData.Msg DD.Verdict)

  -- * do an action according to offer-pattern in the model
, txsStepOffer     -- :: D.Offer -> IOC.IOC (Either EnvData.Msg DD.Verdict)

  -- * run n actions on the model; if n<0 then indefinitely many actions
, txsStepRun       -- :: Int -> IOC.IOC (Either EnvData.Msg DD.Verdict)

  -- * give the menu of possible offers in the model
, txsStepMenu      -- :: IOC.IOC (Either EnvData.Msg BTree.Menu)

  -- * give current state number in the model
, txsStepStateNr   -- :: IOC.IOC (Either EnvData.Msg EnvData.StateNr)

  -- * give current state in the model
, txsStepState     -- :: IOC.IOC (Either EnvData.Msg BTree.BTree)

  -- * give trace from initial state to current state in the model
, txsStepTrace     -- :: IOC.IOC (Either EnvData.Msg [DD.Action])

  -- * give transition graph of actions stepped through so far
, txsStepGraph     -- :: IOC.IOC (Either EnvData.Msg
                   --                    [(EnvData.StateNr,DD.Action,EnvData.StateNr)])

  -- * go to the specified state number in the model
, txsStepTo        -- :: EnvData.StateNr -> IOC.IOC (Either EnvData.Msg ())

  -- * go to the initial state in the model
, txsStepInit      --  :: IOC.IOC (Either EnvData.Msg ())

  -- * go back with the specified number of steps in the model.
, txsStepBack      --  :: Int -> IOC.IOC (Either EnvData.Msg ())
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import qualified Data.Map            as Map

-- import from local
import           CoreUtils
import           Step

-- import from behave(defs)
import qualified Behave
import qualified BTree

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData

-- import from defs
import qualified TxsDDefs            as DD
import qualified TxsDefs             as D


-- ----------------------------------------------------------------------------------------- --
-- | Set Stepping Mode.
--
--   Only possible when in Initing Mode.
txsSetStep :: D.ModelDef                       -- ^ model definition.
           -> IOC.IOC (Either EnvData.Msg ())
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
               Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Stepping Mode set" ]
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping Mode must be set from Initing mode"

-- ----------------------------------------------------------------------------------------- --
-- | Shut Stepping Mode.
--
--   Only possible when in StepSet Mode.
txsShutStep :: IOC.IOC (Either EnvData.Msg ())
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
               Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Stepping Mode shut" ]
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping Mode must be shut from StepSet Mode"
                 
-- ----------------------------------------------------------------------------------------- --
-- | Start stepping.
--
--   Only possible when in StepSet Mode.
txsStartStep :: IOC.IOC (Either EnvData.Msg ())
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
                   -> Right <$> putmsgs [ EnvData.TXS_CORE_SYSTEM_INFO
                                          "Starting Stepping Mode failed"]
                 Just bt
                   -> do IOC.modifyCS $ \st -> st { IOC.modstss = Map.singleton 0 bt }
                         Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                             "Stepping Mode started" ]
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping Mode must be started from StepSet Mode" 
 

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
txsStopStep :: IOC.IOC (Either EnvData.Msg ())
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
               Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Stepping Mode stopped" ]
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping Mode must be stopped from Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Step model with the provided action.
--
--   Only possible when Stepping.
txsStepAct :: DD.Action                           -- ^ Action to step in model.
           -> IOC.IOC (Either EnvData.Msg DD.Verdict)
txsStepAct act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> Right <$> Step.stepA act
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping with action only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Step with action according to offer-pattern in the model.
--
--   Only possible when in Stepping Mode.
txsStepOffer :: D.Offer                           -- ^ Offer-pattern to step in model.
             -> IOC.IOC (Either EnvData.Msg DD.Verdict)
txsStepOffer offer  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.putmsgs = putmsgs }
         -> do mact <- randOff2Act offer
               case mact of
                 Nothing
                   -> do putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Could not generate action for stepping" ]
                         return $ Right DD.NoVerdict
                 Just act
                   -> Right <$> Step.stepA act
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping with offer only in Stepping Mode" 

-- ----------------------------------------------------------------------------------------- --
-- | Step model with the provided number of actions.
--
--   Only possible when Stepping.
txsStepRun :: Int                               -- ^ Number of actions to step model.
           -> IOC.IOC (Either EnvData.Msg DD.Verdict)
txsStepRun nrsteps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> Right <$> Step.stepN nrsteps 1
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping with run only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Give the menu, i.e., all possible offers.
--
--   Only possible when Stepping.
txsStepMenu :: IOC.IOC (Either EnvData.Msg BTree.Menu)
txsStepMenu  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {}
         -> do menuIn  <- Step.stepModelMenuIn
               menuOut <- Step.stepModelMenuOut
               return $ Right $ menuIn ++ menuOut
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Stepping menu only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Give current state number
--
--   Only possible when Stepping.
txsStepStateNr :: IOC.IOC (Either EnvData.Msg EnvData.StateNr)
txsStepStateNr  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.curstate = curstate }
         -> return $ Right curstate
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Current state of stepping only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Give current state 
--
--   Only possible when Stepping.
txsStepState :: IOC.IOC (Either EnvData.Msg BTree.BTree)
txsStepState  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.curstate = curstate
                    , IOC.modstss  = modstss
                    }
         -> do case Map.lookup curstate modstss of
                 Nothing    -> return $ Left $ EnvData.TXS_CORE_SYSTEM_ERROR
                                               "No valid current state"
                 Just btree -> return $ Right btree
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Current state of stepping only in Stepping Mode"
                
-- ----------------------------------------------------------------------------------------- --
-- | Give trace from initial state to current state
--
--   Only possible when Stepping.
txsStepTrace :: IOC.IOC (Either EnvData.Msg [DD.Action])
txsStepTrace  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.behtrie  = behtrie
                    , IOC.inistate = inistate
                    , IOC.curstate = curstate
                    }
         -> case trace behtrie inistate curstate of
              Nothing -> return $ Left $ EnvData.TXS_CORE_SYSTEM_ERROR
                                         "Path error: Behaviour Trie is not a tree"
              Just t  -> return $ Right t
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Trace of stepping only in Stepping Mode"
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
txsStepGraph :: IOC.IOC (Either EnvData.Msg [(EnvData.StateNr, DD.Action, EnvData.StateNr)])
txsStepGraph  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.behtrie  = behtrie }
         -> return $ Right behtrie
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Graph of stepping only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Go to state with the provided state number in the model
--
--   Only possible when Stepping.
txsStepTo :: EnvData.StateNr               -- ^ state to go to.
          -> IOC.IOC (Either EnvData.Msg ())
txsStepTo statenr  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.modstss = modstss
                    , IOC.putmsgs  = putmsgs
                    }
         -> case Map.lookup statenr modstss of
              Nothing -> Right <$> putmsgs [ EnvData.TXS_CORE_USER_ERROR
                                             "No such state to go to" ]
              Just _  -> Right <$> IOC.modifyCS ( \st -> st { IOC.curstate = statenr } )
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Go to stepping state only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Go to initial in the model
--
--   Only possible when Stepping.
txsStepInit :: IOC.IOC (Either EnvData.Msg ())
txsStepInit  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.inistate = inistate
                    , IOC.modstss  = modstss
                    }
         -> case Map.lookup inistate modstss of
              Nothing -> return $ Left $ EnvData.TXS_CORE_SYSTEM_ERROR
                                         "No initial state to go to"
              Just _  -> Right <$> IOC.modifyCS ( \st -> st { IOC.curstate = inistate } )
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Go to initial stepping state only in Stepping Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Go back with the specified number of steps in the model.
--
--   Only possible when Stepping.
txsStepBack :: Int                                -- ^ number of steps for going back.
            -> IOC.IOC (Either EnvData.Msg ())
txsStepBack steps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping { IOC.behtrie  = behtrie
                    , IOC.curstate = curstate
                    , IOC.modstss  = modstss
                    , IOC.putmsgs  = putmsgs
                    }
         -> case Map.lookup curstate modstss of
              Nothing
                -> return $ Left $ EnvData.TXS_CORE_SYSTEM_ERROR
                                   "No state to go backwards from"
              Just _
                -> let newstate = back behtrie curstate steps
                    in case newstate of
                         Nothing
                           -> Right <$> putmsgs [ EnvData.TXS_CORE_USER_ERROR
                                                  "No state to go backwards to" ]
                         Just ns
                           -> case Map.lookup ns modstss of
                                Nothing -> return $ Left $ EnvData.TXS_CORE_SYSTEM_ERROR
                                                           "No state to go backwards to"
                                Just _  -> Right <$>
                                             IOC.modifyCS ( \st -> st { IOC.curstate = ns } )
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "Go backwards in stepping state only in Stepping Mode"
  where
     back :: [(EnvData.StateNr, DD.Action, EnvData.StateNr)]
          -> EnvData.StateNr
          -> Int
          -> Maybe EnvData.StateNr
     back _behtrie _cur stps    | stps <  0  =  Nothing
     back _behtrie  cur stps    | stps == 0  =  Just cur
     back  behtrie  cur stps -- | stps >  0
       =  case [ (s1,a,s2) | (s1,a,s2) <- behtrie, s2 == cur ] of
            [(s1,_a,_s2)] -> back behtrie s1 (stps-1)
            _             -> Nothing

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

