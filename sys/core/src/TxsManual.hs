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

( -- * set manual mode for External World
  mnlSetManual    -- :: IOC.EWorld ew => ew -> IOC.IOC ()      

  -- * shut manual mode for External World
, mnlShutManual   -- :: IOC.IOC ()

  -- * start External World
, mnlStartW       -- :: IOC.IOC ()

  -- * stop External World
, mnlStopW        -- :: IOC.IOC ()

  -- * send action to External World
, mnlActToW       -- :: DD.Action -> IOC.IOC DD.Action

  -- * observe action from External World
, mnlObsFroW      -- :: IOC.IOC DD.Action

  -- * send action according to offer pattern to External World
, mnlOfferToW     -- :: D.Offer -> IOC.IOC DD.Action

  -- * run n actions on External World; if n<0 then infinitely many actions 
, mnlRunW         -- :: Int -> IOC.IOC DD.Verdict

  -- * give current state number
, mnlGetStNr      -- :: IOC.IOC Int

  -- * give path from initial state to current state number
, mnlGetPath      -- :: -> IOC.IOC [(EnvData.StateNr,DD.Action,EnvData.StateNr)]
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import           Control.Arrow
-- import           Control.Monad
import           Control.Monad.State
-- import qualified Data.List           as List
-- import qualified Data.Map            as Map
import           Data.Maybe
-- import           Data.Monoid
-- import qualified Data.Set            as Set
-- import qualified Data.Text           as T
import           System.Random

-- import           Config              (Config)
-- import qualified Config

import CoreUtils
-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData
-- import qualified ParamCore

-- import from defs
import qualified TxsDefs             as D
import qualified TxsDDefs            as DD
import qualified TxsShow


-- ----------------------------------------------------------------------------------------- --

-- | Set manual mode
--
--   Only possible when in initialized mode.
mnlSetManual :: IOC.EWorld ew
             => ew                             -- ^ external world.
             -> IOC.IOC ()                     -- ^ modified external world.
mnlSetManual eWorld  =  do
     envc <- get
     case IOC.state envc of
       IOC.Initing { IOC.smts    = smts
                   , IOC.tdefs   = tdefs
                   , IOC.sigs    = sigs
                   , IOC.putmsgs = putmsgs
                   }
         -> do IOC.putCS IOC.ManualIdle { IOC.smts     = smts
                                        , IOC.tdefs    = tdefs
                                        , IOC.sigs     = sigs
                                        , IOC.eworld   = eWorld
                                        , IOC.putmsgs  = putmsgs
                                        }
               putmsgs [ EnvData.TXS_CORE_USER_INFO "Manual mode set" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Manual mode must be set in Initing mode" ]

-- ----------------------------------------------------------------------------------------- --

-- | Shut manual mode
--
--   Only possible when in manual idle mode.
mnlShutManual :: IOC.IOC ()
mnlShutManual  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualIdle { IOC.smts     = smts
                      , IOC.tdefs    = tdefs
                      , IOC.sigs     = sigs
                      , IOC.eworld   = _eworld
                      , IOC.putmsgs  = putmsgs
                      }
         -> do IOC.putCS IOC.Initing { IOC.smts    = smts
                                     , IOC.tdefs   = tdefs
                                     , IOC.sigs    = sigs
                                     , IOC.putmsgs = putmsgs
                                     }
               putmsgs [ EnvData.TXS_CORE_USER_INFO "Manual mode shut" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Manual mode must be shut from ManualIdle" ]

-- ----------------------------------------------------------------------------------------- --

-- | Start External World
--
--   Only possible when in manual idle mode.
mnlStartW :: IOC.IOC ()
mnlStartW  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualIdle { IOC.smts     = smts
                      , IOC.tdefs    = tdefs
                      , IOC.sigs     = sigs
                      , IOC.eworld   = eworld
                      , IOC.putmsgs  = putmsgs
                      }
         -> do eworld' <- IOC.startW eworld
               IOC.putCS IOC.ManualActive { IOC.smts     = smts
                                          , IOC.tdefs    = tdefs
                                          , IOC.sigs     = sigs
                                          , IOC.behtrie  = []
                                          , IOC.inistate = 0
                                          , IOC.curstate = 0
                                          , IOC.eworld   = eworld'
                                          , IOC.putmsgs  = putmsgs
                                          }
               putmsgs [ EnvData.TXS_CORE_USER_INFO "EWorld started" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "EWorld must be started form ManualIdle" ]

-- ----------------------------------------------------------------------------------------- --

-- | Stop External World
--
--   Only possible when in manual active mode.
mnlStopW :: IOC.IOC ()
mnlStopW  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualActive { IOC.smts     = smts
                        , IOC.tdefs    = tdefs
                        , IOC.sigs     = sigs
                        , IOC.behtrie  = _behtrie
                        , IOC.inistate = _inistate
                        , IOC.curstate = _curstate
                        , IOC.eworld   = eworld
                        , IOC.putmsgs  = putmsgs
                        }
         -> do eworld' <- IOC.stopW eworld
               IOC.putCS IOC.ManualIdle { IOC.smts     = smts
                                        , IOC.tdefs    = tdefs
                                        , IOC.sigs     = sigs
                                        , IOC.eworld   = eworld'
                                        , IOC.putmsgs  = putmsgs
                                        }
               putmsgs [ EnvData.TXS_CORE_USER_INFO "EWorld stopped" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "EWorld must be stopped form ManualActive" ]

-- ----------------------------------------------------------------------------------------- --

-- | Provide action to External World
--
--   Only possible when in manual active mode.
mnlActToW :: DD.Action -> IOC.IOC (Maybe DD.Action)
mnlActToW act  =  do
     envc <- get
     case ( act, IOC.state envc ) of
       ( DD.Act _acts, IOC.ManualActive { IOC.behtrie  = behtrie
                                        , IOC.curstate = curstate
                                        , IOC.eworld   = eworld
                                        , IOC.putmsgs  = putmsgs
                                        }                         )
         -> do act' <- IOC.putToW eworld act
               IOC.modifyCS $ \cs -> cs { IOC.behtrie = behtrie ++ [(curstate,act',curstate+1)]
                                        , IOC.curstate = curstate+1
                                        }
               putmsgs [ EnvData.TXS_CORE_USER_INFO $
                         TxsShow.showN (curstate+1) 6 ++ "  IN: " ++ TxsShow.fshow act' ]
               return $ Just act'
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual input action on EWorld only in ManualActive Mode" ]
               return Nothing

-- ----------------------------------------------------------------------------------------- --

-- | Observe action from External World
--
--   Only possible when in manual active mode.
mnlObsFroW :: IOC.IOC (Maybe DD.Action)
mnlObsFroW  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualActive { IOC.behtrie  = behtrie
                        , IOC.curstate = curstate
                        , IOC.eworld   = eworld
                        , IOC.putmsgs  = putmsgs
                        }
         -> do act' <- IOC.getFroW eworld
               IOC.modifyCS $ \cs -> cs { IOC.behtrie = behtrie ++ [(curstate,act',curstate+1)]
                                        , IOC.curstate = curstate+1
                                        }
               putmsgs [ EnvData.TXS_CORE_USER_INFO $
                         TxsShow.showN (curstate+1) 6 ++ " OUT: " ++ TxsShow.fshow act' ]
               return $ Just act'
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual observation on EWorld only in ManualActive Mode" ]
               return Nothing

-- ----------------------------------------------------------------------------------------- --

-- | Provide action according to offer pattern to External World
--
--   Only possible when in manual active mode.
mnlOfferToW :: D.Offer -> IOC.IOC (Maybe DD.Action)
mnlOfferToW offer  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualActive { IOC.behtrie  = behtrie
                        , IOC.curstate = curstate
                        , IOC.eworld   = eworld
                        , IOC.putmsgs  = putmsgs
                        }
         -> do input <- randOffAct offer
               case input of
                 Nothing  -> do putmsgs [ EnvData.TXS_CORE_USER_INFO "No action to EWorld" ]
                                return Nothing
                 Just act -> do act' <- IOC.putToW eworld act
                                IOC.modifyCS $ \cs -> cs
                                      { IOC.behtrie  = behtrie ++ [(curstate,act',curstate+1)]
                                      , IOC.curstate = curstate+1
                                      }
                                putmsgs [ EnvData.TXS_CORE_USER_INFO $ TxsShow.showN
                                            (curstate+1) 6 ++ "  IN: " ++ TxsShow.fshow act' ]
                                return $ Just act'
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual offer on EWorld only in ManualActive Mode" ]
               return Nothing
 
-- ----------------------------------------------------------------------------------------- --

-- | Run a number of random actions on External World
--
--   Only possible when in manual active mode.
mnlRunW :: Int -> IOC.IOC DD.Verdict
mnlRunW nrsteps  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualActive { IOC.eworld  = eworld }
         -> mnlRunW' (IOC.chansToW eworld) nrsteps False
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Run on EWorld only in ManualActive Mode" ]
               return DD.NoVerdict
  where
     mnlRunW' :: [D.ChanId] -> Int -> Bool -> IOC.IOC DD.Verdict
     mnlRunW' chans depth lastDelta  =
          if  depth == 0
            then return DD.Pass
            else do
              ioRand <- lift $ randomRIO (False,True)
              input <- randAct chans
              if isJust input && ( lastDelta || ioRand )
                then do                                                         -- try input --
                  let Just act = input
                  act' <- mnlActToW act
                  case act' of
                    Nothing    -> return DD.NoVerdict
                    Just act'' -> mnlRunW' chans (depth-1) (act''==DD.ActQui)
                else
                  if not lastDelta
                    then do                                                -- observe output --
                      act' <- mnlObsFroW
                      case act' of
                        Nothing    -> return DD.NoVerdict
                        Just act'' -> mnlRunW' chans (depth-1) (act''==DD.ActQui)
                    else do                                 -- lastDelta and no inputs: stop --
                      IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "No more actions on EWorld" ]
                      return DD.Pass
 
-- ----------------------------------------------------------------------------------------- --

-- | Give current state number
--
--   Only possible when in manual active mode.
mnlGetStNr :: IOC.IOC Int
mnlGetStNr  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualActive { IOC.curstate = curstate }
         -> return curstate
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Current state of EWorld only in ManualActive Mode" ]
               return $ -1
    
-- ----------------------------------------------------------------------------------------- --

-- | Give path from initial state to current state number
--
--   Only possible when in manual active mode.
mnlGetPath :: IOC.IOC [(EnvData.StateNr,DD.Action,EnvData.StateNr)]
mnlGetPath  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManualActive { IOC.behtrie = behtrie }
         -> return behtrie
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Path of EWorld only in ManualActive Mode" ]
               return []


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

