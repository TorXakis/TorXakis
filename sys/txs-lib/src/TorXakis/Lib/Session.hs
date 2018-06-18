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
import           Control.Concurrent.MVar       (MVar, newMVar)
import           Control.Concurrent.STM.TChan  (TChan, newTChanIO)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import           Control.Concurrent.STM.TVar   (TVar, newTVarIO)
import           Control.DeepSeq               (NFData)
import           Control.Exception             (SomeException)
import qualified Data.Map.Strict               as Map
import           Data.Time                     (UTCTime)
import           GHC.Generics                  (Generic)
import           Lens.Micro.TH                 (makeLenses)
import qualified Network.TextViaSockets        as TVS

import           ChanId                        (ChanId)
import           ConstDefs                     (Const)
import           EnvCore                       (EnvC, initEnvC)
import           EnvData                       (Msg)
import           ParamCore                     (Params)
import           TxsDDefs                      (Action, Verdict)
import           TxsDefs                       (VEnv)
import           VarId                         (VarId)

import           TorXakis.Lib.Common
import           TorXakis.Lib.SessionParams

newtype ToWorldMapping = ToWorldMapping
    { _sendToW         :: [Const] -> IO (Response (Maybe Action))
    }
makeLenses ''ToWorldMapping

data WorldConnDef = WorldConnDef
    { _toWorldConns    :: [TVS.Connection]
    , _toWorldMappings :: Map.Map ChanId ToWorldMapping
    , _fromWorldConns  :: [TVS.Connection]
    , _worldListeners  :: [ThreadId]
    }
makeLenses ''WorldConnDef

data SessionSt = SessionSt
    { _envCore       :: EnvC
    , _sessionParams :: Params
    } deriving (Generic, NFData)

makeLenses ''SessionSt

-- | The session, which maintains the state of a TorXakis model.
data Session = Session
    { _sessionState  :: TVar SessionSt
    , _sessionMsgs   :: TQueue Msg
    , _pendingIOC    :: MVar () -- ^ Signal that a pending IOC operation is taking place.
    , _verdicts      :: TQueue (Either SomeException Verdict)
    , _fromWorldChan :: TChan Action
    , _wConnDef      :: TVar WorldConnDef
    , _timers        :: TVar (Map.Map String UTCTime)
    , _locVars       :: TVar [VarId] -- ^ local free variables
    , _locValEnv     :: TVar VEnv    -- ^ local value environment
    }

makeLenses ''Session

-- * Session state manipulation
emptySessionState :: SessionSt
emptySessionState = SessionSt initEnvC initSessionParams

-- | Create a new session.
newSession :: IO Session
newSession = Session <$> newTVarIO emptySessionState
                     <*> newTQueueIO
                     <*> newMVar ()
                     <*> newTQueueIO
                     <*> newTChanIO
                     <*> newTVarIO (WorldConnDef [] Map.empty [] [])
                     <*> newTVarIO Map.empty
                     <*> newTVarIO []
                     <*> newTVarIO Map.empty

-- | Stop a session.
killSession :: Session -> IO (Response ())
killSession _ =
    return $ Left "Kill Session: Not implemented (yet)"
