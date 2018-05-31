{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveGeneric #-}
module Endpoints.NewSession
    ( NewSessionEP
    , newSrvSession
    ) where

import           Control.Concurrent.STM.TVar (modifyTVar, readTVarIO, writeTVar, newTVarIO)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  (ToJSON)
import qualified Data.IntMap.Strict          as Map
import           GHC.Generics                (Generic)
import           Servant

import           TorXakis.Lib                (newSession)

import           Common                      (Env (..), ServerSession(ServerSession))

newtype SessionIdResponse = SessionIdResponse{sessionId :: Int}
    deriving (Generic)
instance ToJSON SessionIdResponse

type NewSessionEP = "sessions" :> "new" :> PostCreated '[JSON] SessionIdResponse

newSrvSession :: Env -> Handler SessionIdResponse
newSrvSession Env{sessions = ssT, lastSid = pSIdT} = do
    pSId       <- liftIO $ readTVarIO pSIdT
    s <- liftIO newSession
    l <- liftIO $ newTVarIO False
    let sId = pSId + 1
        svS = ServerSession s l
    liftIO $ atomically $ modifyTVar ssT   (Map.insert sId svS)
    liftIO $ atomically $ writeTVar  pSIdT sId
    return SessionIdResponse{sessionId = sId}
