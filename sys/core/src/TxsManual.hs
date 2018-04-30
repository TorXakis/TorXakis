{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- |
-- Module      :  TxsManual
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- Manual Mode
--
-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}

module TxsManual

-- ----------------------------------------------------------------------------------------- --
-- export

(
  -- * set manual mode for External World
  txsSetManual     -- :: IOC.EWorld ew => ew -> IOC.IOC ()

  -- * shut manual mode for External World
, txsShutManual    -- :: IOC.IOC ()

  -- * start running with the External World
,  txsStartW       -- :: IOC.IOC ()

  -- * stop running with the External World
, txsStopW         -- :: IOC.IOC ()

  -- * send an action to the External World, and do that action, or an observed earlier action.
, txsActToW        -- :: DD.Action -> IOC.IOC DD.Verdict

  -- * observe an action from the External World
, txsObsFroW       -- :: IOC.IOC DD.Verdict

  -- send an action according to the offer-pattern to the External World
, txsOfferToW      -- :: D.Offer -> IOC.IOC DD.Verdict

  -- * run n actions on the External World; if n<0 then run indefinitely
, txsRunW          -- :: Int -> IOC.IOC DD.Verdict

  -- * give the current state number
, txsGetWStateNr   -- :: IOC.IOC Int

  -- * give the trace from the initial state to the current state
, txsGetWTrace     -- :: IOC.IOC [DD.Action]
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
-- | Set Manual Mode
--
--   Only possible when in Initing Mode.
txsSetManual :: IOC.EWorld ew
             => ew
             -> IOC.IOC ()
txsSetManual eworld  =  do
     envc <- get
     case IOC.state envc of
       IOC.Initing { IOC.smts    = smts
                   , IOC.tdefs   = tdefs
                   , IOC.sigs    = sigs
                   , IOC.putmsgs = putmsgs
                   }
         -> do IOC.putCS IOC.ManSet { IOC.smts     = smts
                                    , IOC.tdefs    = tdefs
                                    , IOC.sigs     = sigs
                                    , IOC.eworld   = eworld
                                    , IOC.putmsgs  = putmsgs
                                    }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Manual Mode set" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Manual Mode must be set in Initing mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Shut Manual Mode
--
--   Only possible when in ManSet Mode.
txsShutManual :: IOC.IOC ()
txsShutManual  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManSet { IOC.smts     = smts
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
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Manual Mode shut" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "Manual Mode must be shut from ManSet Mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Start External World
--
--   Only possible when in ManSet Mode.
txsStartW :: IOC.IOC ()
txsStartW  =  do
     envc <- get
     case IOC.state envc of
       IOC.ManSet { IOC.smts     = smts
                  , IOC.tdefs    = tdefs
                  , IOC.sigs     = sigs
                  , IOC.eworld   = eworld
                  , IOC.putmsgs  = putmsgs
                  }
         -> do eworld' <- IOC.startW eworld
               IOC.putCS IOC.Manualing { IOC.smts     = smts
                                       , IOC.tdefs    = tdefs
                                       , IOC.sigs     = sigs
                                       , IOC.behtrie  = []
                                       , IOC.inistate = 0
                                       , IOC.curstate = 0
                                       , IOC.eworld   = eworld'
                                       , IOC.putmsgs  = putmsgs
                                       }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "EWorld started" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "EWorld must be started from ManSet Mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Stop External World
--
--   Only possible when in Manualing Mode.
txsStopW :: IOC.IOC ()
txsStopW  =  do
     envc <- get
     case IOC.state envc of
       IOC.Manualing { IOC.smts     = smts
                     , IOC.tdefs    = tdefs
                     , IOC.sigs     = sigs
                     , IOC.behtrie  = _behtrie
                     , IOC.inistate = _inistate
                     , IOC.curstate = _curstate
                     , IOC.eworld   = eworld
                     , IOC.putmsgs  = putmsgs
                     }
         -> do eworld' <- IOC.stopW eworld
               IOC.putCS IOC.ManSet { IOC.smts     = smts
                                    , IOC.tdefs    = tdefs
                                    , IOC.sigs     = sigs
                                    , IOC.eworld   = eworld'
                                    , IOC.putmsgs  = putmsgs
                                    }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "EWorld stopped" ]
       _ -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "EWorld must be stopped from Manualing Mode" ]

-- ----------------------------------------------------------------------------------------- --
-- | Provide action to External World
--
--   Only possible when in Manualing Mode.
txsActToW :: DD.Action -> IOC.IOC DD.Verdict
txsActToW act  =  do
     envc <- get
     case ( act, IOC.state envc ) of
       ( DD.Act _acts, IOC.Manualing { IOC.behtrie  = behtrie
                                     , IOC.curstate = curstate
                                     , IOC.eworld   = eworld
                                     , IOC.putmsgs  = putmsgs
                                     }                         )
         -> do act' <- IOC.putToW eworld act
               IOC.modifyCS $ \cs -> cs { IOC.behtrie = behtrie ++ [(curstate,act',curstate+1)]
                                        , IOC.curstate = curstate+1
                                        }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                           $ TxsShow.showN (curstate+1) 6 ++
                             "  IN: " ++ TxsShow.fshow act'
                           ]
               return $ DD.Pass
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual input on EWorld only in Manualing Mode" ]
               return DD.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- | Observe action from External World
--
--   Only possible when in Manualing Mode.
txsObsFroW :: IOC.IOC DD.Verdict
txsObsFroW  =  do
     envc <- get
     case IOC.state envc of
       IOC.Manualing { IOC.behtrie  = behtrie
                     , IOC.curstate = curstate
                     , IOC.eworld   = eworld
                     , IOC.putmsgs  = putmsgs
                     }
         -> do act' <- IOC.getFroW eworld
               IOC.modifyCS $ \cs -> cs { IOC.behtrie = behtrie ++ [(curstate,act',curstate+1)]
                                        , IOC.curstate = curstate+1
                                        }
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                           $ TxsShow.showN (curstate+1) 6 ++ " OUT: " ++ TxsShow.fshow act'
                           ]
               return $ DD.Pass
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual observation on EWorld only in Manualing Mode" ]
               return DD.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- | Provide action according to offer pattern to External World
--
--   Only possible when in Manualing Mode.
txsOfferToW :: D.Offer -> IOC.IOC DD.Verdict
txsOfferToW offer  =  do
     envc <- get
     case IOC.state envc of
       IOC.Manualing { IOC.behtrie  = behtrie
                     , IOC.curstate = curstate
                     , IOC.eworld   = eworld
                     , IOC.putmsgs  = putmsgs
                     }
         -> do input <- randOff2Act offer
               case input of
                 Nothing
                   -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                       "Could not generate action to EWorld" ]
                         return DD.NoVerdict
                 Just act
                   -> do act' <- IOC.putToW eworld act
                         IOC.modifyCS $ \cs -> cs
                               { IOC.behtrie  = behtrie ++ [(curstate,act',curstate+1)]
                               , IOC.curstate = curstate+1
                               }
                         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                     $ TxsShow.showN (curstate+1) 6 ++
                                       "  IN: " ++ TxsShow.fshow act'
                                     ]
                         return $ DD.Pass
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Manual offer on EWorld only in Manualing Mode" ]
               return DD.NoVerdict
 
-- ----------------------------------------------------------------------------------------- --
-- | Run a number of random actions on External World
--
--   Only possible when in Manualing Mode.
txsRunW :: Int -> IOC.IOC DD.Verdict
txsRunW nrsteps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Manualing { IOC.eworld  = eworld }
         -> txsRunW' (IOC.chansToW eworld) nrsteps False
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Run on EWorld only in ManualActive Mode" ]
               return DD.NoVerdict
  where
     txsRunW' :: [D.ChanId] -> Int -> Bool -> IOC.IOC DD.Verdict
     txsRunW' chans depth lastDelta  =
          if  depth == 0
            then return DD.Pass
            else do
              ioRand <- lift $ randomRIO (False,True)
              input <- randAct chans
              if isJust input && ( lastDelta || ioRand )
                then do                                                         -- try input --
                  let Just act = input
                  verd <- txsActToW act
                  case verd of
                    Just act'' -> txsRunW' chans (depth-1) (act''==DD.ActQui)
                    _          -> return verd
                else
                  if not lastDelta
                    then do                                                -- observe output --
                      act' <- txsObsFroW
                      case act' of
                        Nothing    -> return DD.NoVerdict
                        Just act'' -> txsRunW' chans (depth-1) (act''==DD.ActQui)
                    else do                                 -- lastDelta and no inputs: stop --
                      IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                    "No more actions on EWorld" ]
                      return DD.Pass
 
-- ----------------------------------------------------------------------------------------- --
-- | Give current state number
--
--   Only possible when in Manualing Mode.
txsGetWStateNr :: IOC.IOC EnvData.StateNr
txsGetWStateNr  =  do
     envc <- get
     case IOC.state envc of
       IOC.Manualing { IOC.curstate = curstate }
         -> return curstate
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Current state of EWorld only in Manualing Mode" ]
               return $ -1
    
-- ----------------------------------------------------------------------------------------- --
-- | Give trace from initial state to current state
--
--   Only possible when in Manualing Mode.
txsGetWTrace :: IOC.IOC [DD.Action]
txsGetWTrace  =  do
     envc <- get
     case IOC.state envc of
       IOC.Manualing { IOC.behtrie = behtrie }
         -> return [ act | (s1,act,s2) <- behtrie ]
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Trace of EWorld only in Manualing Mode" ]
               return []

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

