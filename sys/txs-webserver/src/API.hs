{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module API
( startApp
, app
) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import qualified Data.IntMap.Strict          as Map
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

import           Common                      (Env (..))
import           Endpoints.Info              (InfoEP, getInfo)
import           Endpoints.Messages          (CloseMessagesEP, MessagesEP,
                                              OpenMessagesEP, closeMessages,
                                              messages, openMessages)
import           Endpoints.NewSession        (NewSessionEP, newSrvSession)
import           Endpoints.Stepper           (SetStepperEP, StartStepperEP,
                                              TakeNStepsEP, setStep, startStep,
                                              takeNSteps)
import           Endpoints.Tester            (StartTesterEP, TestNStepsEP,
                                              startTester, testNSteps)
import           Endpoints.Upload            (UploadEP, upload)
-- import           Swagger

type API = ServiceAPI
type ServiceAPI
    =    InfoEP
    :<|> NewSessionEP
    :<|> UploadEP
    :<|> SetStepperEP
    :<|> StartStepperEP
    :<|> TakeNStepsEP
    :<|> StartTesterEP
    :<|> TestNStepsEP
    :<|> OpenMessagesEP
    :<|> CloseMessagesEP
    :<|> MessagesEP
    :<|> TestPageAPI
type TestPageAPI = "test" :> Raw

startApp :: Port -> IO ()
startApp p = do
    sessionsMap <- newTVarIO Map.empty
    zeroId      <- newTVarIO 0
    run p $ app $ Env sessionsMap zeroId

app :: Env -> Application
app env = simpleCors $ serve api (server env)
    where
        api :: Proxy API
        api = Proxy

server :: Env -> ServerT API Handler
server env
    =    getInfo
    :<|> newSrvSession env
    :<|> upload env
    :<|> setStep env :<|> startStep env :<|> takeNSteps env
    :<|> startTester env  :<|> testNSteps env
    :<|> openMessages env
    :<|> closeMessages env
    :<|> messages env
    :<|> serveDirectoryWebApp "sys/txs-webserver/test/testPage"
    -- :<|> return swaggerDocs
    -- where
        -- swaggerDocs :: Swagger
        -- swaggerDocs = toSwagger serviceAPI
        --     where serviceAPI :: Proxy ServiceAPI
        --           serviceAPI = Proxy
