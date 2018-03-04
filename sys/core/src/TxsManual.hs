{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  TxsManual
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- API for TorXakis core functionality.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module TxsManual
( -- * set manual mode for external world
  txsSetMan

  -- * start external world manually
, txsManStart

  -- * stop external world manually
, txsManStop

  -- * send action to external world manually
, txsManPutToW

  -- * observe action from external world manually
, txsManGetFroW
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import           Control.Arrow
-- import           Control.Monad
-- import           Control.Monad.State
-- import qualified Data.List           as List
-- import qualified Data.Map            as Map
-- import           Data.Maybe
-- import           Data.Monoid
-- import qualified Data.Set            as Set
-- import qualified Data.Text           as T
-- import           System.Random

-- import from local
-- import           CoreUtils
-- import           Ioco
-- import           Mapper
-- import           NComp
-- import           Purpose
-- import           Sim
-- import           Step
-- import           Test
-- 
-- import           Config              (Config)
-- import qualified Config

-- -- import from behave(defs)
-- import qualified Behave
-- import qualified BTree
-- import           Expand              (relabel)

-- import from coreenv
-- import qualified EnvCore             as IOC
-- -- import qualified EnvData
-- import qualified ParamCore

-- import from defs
-- import qualified Sigs
-- import qualified TxsDDefs
-- import qualified TxsDefs
-- import qualified TxsShow
-- import           TxsUtils

-- import from solve
-- import qualified FreeVar
-- import qualified SMT
-- import qualified Solve
-- import qualified SolveDefs
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

-- | Start manual mode
--
--   Only possible when txscore is initialized.
txsSetMan :: IOC.EWorld ew
          => ew                             -- ^ external world.
          -> IOC.IOC ew                     -- ^ modified external world.
txsSetMan eWorld  =  do
     envc <- get
     let cState = IOC.state envc
     case cState of
       IOC.Initing { IOC.smts    = smts
                   , IOC.tdefs   = tdefs
                   , IOC.sigs    = sigs
                   , IOC.putmsgs = putmsgs
                   }
         -> do IOC.putCS IOC.Manualing { IOC.smts      = smts
                                       , IOC.tdefs     = tdefs
                                       , IOC.sigs      = sigs
                                       , IOC.eworld    = eWorld
                                       , IOC.putmsgs   = putmsgs
                                       }
       _ -> do                        -- IOC.Noning, IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual mode must start in Initing mode" ]
               return eWorld

-- ----------------------------------------------------------------------------------------- --

-- | Start External World
--
--   Only possible when txscore is in manualing mode.
txsManStartW :: IOC.IOC ()
txsManStartW  =  do
     envc <- get
     let cState = IOC.state envc
     case cState of
       IOC.Manualing { IOC.smts    = smts
                     , IOC.tdefs   = tdefs
                     , IOC.sigs    = sigs
                     , IOC.eworld  = eworld
                     , IOC.putmsgs = putmsgs
                     }
         -> do eworld' <- IOC.startW eworld
               IOC.putCS IOC.Manualing { IOC.smts    = smts
                                       , IOC.tdefs   = tdefs
                                       , IOC.sigs    = sigs
                                       , IOC.eworld  = eworld'
                                       , IOC.putmsgs = putmsgs
                                       } 
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "EWorld started" ]
       _ ->                           -- IOC.Noning, IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual operations on eworld only in Manualing Mode" ]

-- ----------------------------------------------------------------------------------------- --

-- | Stop External World
--
--   Only possible when txscore is in manualing mode.
txsManStopW :: IOC.IOC ()
txsManStopW  =  do
     envc <- get
     let cState = IOC.state envc
     case cState of
       IOC.Manualing { IOC.smts    = smts
                     , IOC.tdefs   = tdefs
                     , IOC.sigs    = sigs
                     , IOC.eworld  = eworld
                     , IOC.putmsgs = putmsgs
                     }
         -> do eworld' <- IOC.stopW eworld
               IOC.putCS IOC.Manualing { IOC.smts    = smts
                                       , IOC.tdefs   = tdefs
                                       , IOC.sigs    = sigs
                                       , IOC.eworld  = eworld'
                                       , IOC.putmsgs = putmsgs
                                       } 
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "EWorld stopped" ]
       _ ->                           -- IOC.Noning, IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual operations on eworld only in Manualing Mode" ]

-- ----------------------------------------------------------------------------------------- --

-- | Provide action to External World
--
--   Only possible when txscore is in manualing mode.
txsManPutToW :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
txsManPutToW act  =  do
     envc <- get
     let cState = IOC.state envc
     case ( act, cState ) of
       ( Act acts, IOC.Manualing { IOC.eworld  = eworld } )
         -> do act' <- IOC.putToW eworld act
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Action done: " ++ pshow act' ]
       _ ->                           -- IOC.Noning, IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual action -no quiescence- on eworld only in Manualing Mode" ]

-- ----------------------------------------------------------------------------------------- --

-- | Observe action from External World
--
--   Only possible when txscore is in manualing mode.
txsManGetFroW :: IOC.IOC TxsDDefs.Action
txsManGetFroW  =  do
     envc <- get
     let cState = IOC.state envc
     case cState of
       IOC.Manualing { IOC.eworld  = eworld }
         -> do act' <- IOC.getFroW eworld
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Action observed: " ++ pshow act' ]
       _ ->                           -- IOC.Noning, IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual action -no quiescence- on eworld only in Manualing Mode" ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

