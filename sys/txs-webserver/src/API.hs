{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
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
import           Endpoints.Messages          (MessagesEP, SSMessagesEP,
                                              ssMessagesEP, streamMessages)
import           Endpoints.NewSession        (NewSessionEP, newSrvSession)
import           Endpoints.Stepper           (StartStepperEP, TakeNStepsEP,
                                              startStepper, takeNSteps)
import           Endpoints.Upload            (UploadEP, upload)
-- import           Swagger

type API = ServiceAPI
type ServiceAPI = NewSessionEP :<|> UploadEP :<|> StartStepperEP :<|> TakeNStepsEP :<|> MessagesEP
                               :<|> SSMessagesEP
                               :<|> InfoEP
                               :<|> TestPageAPI
type TestPageAPI = "test" :> Raw

startApp :: IO ()
startApp = do
    sessionsMap <- newTVarIO Map.empty
    zeroId      <- newTVarIO 0
    run 8080 $ app $ Env sessionsMap zeroId

app :: Env -> Application
app env = simpleCors $ serve api (server env)
    where
        api :: Proxy API
        api = Proxy

server :: Env -> ServerT API Handler
server env = newSrvSession env
    :<|> upload env
    :<|> startStepper env
    :<|> takeNSteps env
    :<|> streamMessages env
    :<|> ssMessagesEP env
    :<|> getInfo
    :<|> serveDirectoryWebApp "sys/txs-webserver/test/testPage"
    -- :<|> return swaggerDocs
    -- where
        -- swaggerDocs :: Swagger
        -- swaggerDocs = toSwagger serviceAPI
        --     where serviceAPI :: Proxy ServiceAPI
        --           serviceAPI = Proxy
