
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

import           Data.Text    (Text)
import           Servant

import           TorXakis.Lib (StepType (..), test, tester)

import           Common       (Env, SessionId, liftLib)

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
startTester env sId model = liftLib env sId (`tester` model)

testNSteps :: Env -> SessionId -> Int -> Handler ()
testNSteps env sId steps = liftLib env sId (`test` NumberOfSteps steps)
