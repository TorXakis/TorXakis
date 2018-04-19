{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Endpoints.NewSession
    ( NewSessionEP
    , newSrvSession
    ) where

import           Control.Concurrent.STM.TVar (modifyTVar, readTVarIO, writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import qualified Data.IntMap.Strict          as Map
import           Servant

import           TorXakis.Lib     (newSession)

import           Common (SessionId, Env(..))

type NewSessionEP = "sessions" :> "new" :> PostCreated '[JSON] SessionId

newSrvSession :: Env -> Handler SessionId
newSrvSession Env{sessions = ssT, lastSid = pSidT} = do
    pSid       <- liftIO $ readTVarIO pSidT
    s <- liftIO newSession
    let sid = pSid + 1
    liftIO $ atomically $ modifyTVar ssT   (Map.insert sid s)
    liftIO $ atomically $ writeTVar  pSidT sid
    return sid
