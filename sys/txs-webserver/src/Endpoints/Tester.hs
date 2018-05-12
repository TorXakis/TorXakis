{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Endpoints.Tester
( StartTesterEP
, startTester
, TestNStepsEP
, testNSteps
) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text)
import           Servant

import           TorXakis.Lib                (tester, test, StepType (..))

import           Common (SessionId, getSession, Env, checkResult)

type StartTesterEP  = "tester"
                   :> "start"
                   :> Capture "sid" SessionId
                   :> Capture "model" Text
                   :> Post '[JSON] ()
                      
type TestNStepsEP   = "tester"
                   :> "test"
                   :> Capture "sid" SessionId
                   :> Capture "steps" Int
                   :> Post '[JSON] ()

startTester :: Env -> SessionId -> Text -> Handler ()
startTester env sid model = do
    s <- getSession env sid
    r <- liftIO $ tester s model
    checkResult r

testNSteps :: Env -> SessionId -> Int -> Handler ()
testNSteps env sid steps = do
    s <- getSession env sid
    r <- liftIO $ test s $ NumberOfSteps steps
    checkResult r

