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

import           Control.Concurrent.STM.TVar (modifyTVar, readTVarIO, writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  (ToJSON)
import qualified Data.IntMap.Strict          as Map
import           GHC.Generics                (Generic)
import           Servant

import           TorXakis.Lib                (newSession)

import           Common                      (Env (..))

newtype SessionIdResponse = SessionIdResponse{sessionId :: Int}
    deriving (Generic)
instance ToJSON SessionIdResponse

type NewSessionEP = "sessions" :> "new" :> PostCreated '[JSON] SessionIdResponse

newSrvSession :: Env -> Handler SessionIdResponse
newSrvSession Env{sessions = ssT, lastSid = pSidT} = do
    pSid       <- liftIO $ readTVarIO pSidT
    s <- liftIO newSession
    let sid = pSid + 1
    liftIO $ atomically $ modifyTVar ssT   (Map.insert sid s)
    liftIO $ atomically $ writeTVar  pSidT sid
    return SessionIdResponse{sessionId = sid}
