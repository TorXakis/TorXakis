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
import           Endpoints.Eval              (EvalEP, eval)
import           Endpoints.Info              (InfoEP, getInfo)
import           Endpoints.LPE               (LpeEP, lpe)
import           Endpoints.Menu              (MenuEP, menu)
import           Endpoints.Messages          (CloseMessagesEP, MessagesEP,
                                              OpenMessagesEP, closeMessages,
                                              messages, openMessages)
import           Endpoints.NewSession        (NewSessionEP, newSrvSession)
import           Endpoints.NComp             (NCompEP, ncomp)
import           Endpoints.Params            (ParamsAPI, paramsServer)
import           Endpoints.Parse             (ParseActionEP, parseAction)
import           Endpoints.Seed              (SeedEP, setSeed)
import           Endpoints.Show              (ShowAPI, showServer)
import           Endpoints.Simulator         (SetSimEP, SimStepEP, simStep,
                                              startSimulator)
import           Endpoints.Solve             (SolveEP, solve)
import           Endpoints.States            (StatesEP, states)
import           Endpoints.Stepper           (SetStepperEP, StepEP, setStep,
                                              step)
import           Endpoints.Stop              (StopEP, stop)
import           Endpoints.Tester            (SetTestEP, TestOutEP, TestStepEP,
                                              startTester, testOut, testStep)
import           Endpoints.Time              (TimeEP, getTime)
import           Endpoints.Timer             (TimerEP, timer)
import           Endpoints.Upload            (UploadEP, upload)
import           Endpoints.Vals              (ValsAPI, valsServer)
import           Endpoints.Vars              (VarsAPI, varsServer)

type API = ServiceAPI
type ServiceAPI
    =    InfoEP
    :<|> TimeEP
    :<|> NewSessionEP
    :<|> TimerEP
    :<|> ParamsAPI
    :<|> UploadEP
    :<|> SetStepperEP
    :<|> StepEP
    :<|> SetTestEP
    :<|> TestStepEP
    :<|> TestOutEP
    :<|> SetSimEP
    :<|> SimStepEP
    :<|> ParseActionEP
    :<|> OpenMessagesEP
    :<|> CloseMessagesEP
    :<|> MessagesEP
    :<|> ValsAPI
    :<|> VarsAPI
    :<|> EvalEP
    :<|> SolveEP
    :<|> SeedEP
    :<|> StopEP
    :<|> MenuEP
    :<|> ShowAPI
    :<|> LpeEP
    :<|> NCompEP
    :<|> StatesEP
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
    :<|> step env
    :<|> startTester env
    :<|> testStep env
    :<|> testOut env
    :<|> startSimulator env
    :<|> simStep env
    :<|> parseAction env
    :<|> openMessages env
    :<|> closeMessages env
    :<|> messages env
    :<|> valsServer env
    :<|> varsServer env
    :<|> eval env
    :<|> solve env
    :<|> setSeed env
    :<|> stop env
    :<|> menu env
    :<|> showServer env
    :<|> lpe env
    :<|> ncomp env
    :<|> states env
    :<|> serveDirectoryWebApp "sys/txs-webserver/test/testPage"
