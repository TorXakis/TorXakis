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
import           Endpoints.Messages          (CloseMessagesEP, MessagesEP,
                                              OpenMessagesEP, closeMessages,
                                              messages, openMessages)
import           Endpoints.NewSession        (NewSessionEP, newSrvSession)
import           Endpoints.Params            (ParamsAPI, paramsServer)
import           Endpoints.Parse             (ParseActionEP, parseAction)
import           Endpoints.Stepper           (SetStepperEP, StartStepperEP,
                                              StepEP, setStep, startStep, step)
import           Endpoints.Tester            (StartTesterEP, TestNStepsEP,
                                              startTester, testNSteps)
import           Endpoints.Time              (TimeEP, getTime)
import           Endpoints.Timer             (TimerEP, timer)
import           Endpoints.Upload            (UploadEP, upload)
import           Endpoints.Vals              (ValsAPI, valsServer)
import           Endpoints.Vars              (VarsAPI, varsServer)
import           Endpoints.Eval              (EvalEP, eval)

type API = ServiceAPI
type ServiceAPI
    =    InfoEP
    :<|> TimeEP
    :<|> NewSessionEP
    :<|> TimerEP
    :<|> ParamsAPI
    :<|> UploadEP
    :<|> SetStepperEP
    :<|> StartStepperEP
    :<|> StepEP
    :<|> StartTesterEP
    :<|> TestNStepsEP
    :<|> ParseActionEP
    :<|> OpenMessagesEP
    :<|> CloseMessagesEP
    :<|> MessagesEP
    :<|> ValsAPI
    :<|> VarsAPI
    :<|> EvalEP
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
    :<|> getTime
    :<|> newSrvSession env
    :<|> timer env
    :<|> paramsServer env
    :<|> upload env
    :<|> setStep env
    :<|> startStep env
    :<|> step env
    :<|> startTester env
    :<|> testNSteps env
    :<|> parseAction env
    :<|> openMessages env
    :<|> closeMessages env
    :<|> messages env
    :<|> valsServer env
    :<|> varsServer env
    :<|> eval env
    :<|> serveDirectoryWebApp "sys/txs-webserver/test/testPage"
