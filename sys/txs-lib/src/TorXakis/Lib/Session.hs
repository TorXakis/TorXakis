{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- |
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TorXakis.Lib.Session where

import           Control.Concurrent            (ThreadId)
import           Control.Concurrent.MVar       (MVar)
import           Control.Concurrent.STM.TChan  (TChan)
import           Control.Concurrent.STM.TQueue (TQueue)
import           Control.Concurrent.STM.TVar   (TVar)
import           Control.DeepSeq               (NFData)
import           Control.Exception             (SomeException)
import qualified Data.Map.Strict               as Map
import           Data.Time                     (UTCTime)
import           GHC.Generics                  (Generic)
import           Lens.Micro.TH                 (makeLenses)

import           ChanId                        (ChanId)
import           ConstDefs                     (Const)
import           EnvCore                       (EnvC, initEnvC)
import           EnvData                       (Msg)
import           ParamCore                     (Params)
import           TxsDDefs                      (Action, ConnHandle, Verdict)
import           TxsDefs                       (VEnv)
import           VarId                         (VarId)

import           TorXakis.Lib.SessionParams

data ToWorldMapping = ToWorldMapping
    { -- Send some data to the external world, maybe get some action as a response
      _sendToW    :: [Const] -> IO (Maybe Action)
    , _connHandle :: ConnHandle
    }
makeLenses ''ToWorldMapping

-- | TODO: put in the right place:
newtype WorldConnDef = WorldConnDef
    { _toWorldMappings :: Map.Map ChanId ToWorldMapping
    }
makeLenses ''WorldConnDef

-- TODO: '_tdefs' '_sigs', and '_wConnDef' should be placed in a data structure
-- having a name like 'SessionEnv', since they won't change once a 'TorXakis'
-- file is compiled.
data SessionSt = SessionSt
    { _envCore       :: EnvC
    , _sessionParams :: Params
    } deriving (Generic, NFData)

makeLenses ''SessionSt

-- | The session, which maintains the state of a TorXakis model.
data Session = Session
    { _sessionState   :: TVar SessionSt
    , _sessionMsgs    :: TQueue Msg
    , _pendingIOC     :: MVar () -- ^ Signal that a pending IOC operation is taking place.
    , _verdicts       :: TQueue (Either SomeException Verdict)
    , _fromWorldChan  :: TChan Action
    , _wConnDef       :: TVar WorldConnDef
    , _worldListeners :: TVar [ThreadId]
    , _timers         :: TVar (Map.Map String UTCTime)
    , _locVars        :: TVar [VarId] -- ^ local free variables
    , _locValEnv      :: TVar VEnv    -- ^ local value environment
    }

makeLenses ''Session

-- * Session state manipulation
emptySessionState :: SessionSt
emptySessionState = SessionSt initEnvC initSessionParams
