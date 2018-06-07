
{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.Tester where

import           Data.Text    (Text)
import           Servant

import           TorXakis.Lib (StepType (..), setTest, test, testO)

import           Common       (Env, SessionId, liftLib)

type SetTestEP = "sessions"
              :> Capture "sid" SessionId
              :> "set-test"
              :> Capture "model" Text
              :> Capture "cnect" Text
              :> PostNoContent '[JSON] ()

startTester :: Env -> SessionId -> Text -> Text -> Handler ()
startTester env sId model cnect= liftLib env sId (setTest model cnect)

type TestStepEP = "sessions"
           :> Capture "sid" SessionId
           :> "test"
           :> ReqBody '[JSON] StepType
           :> PostNoContent '[JSON] ()

testStep :: Env -> SessionId -> StepType -> Handler ()
testStep env sId sType = liftLib env sId (`test` sType)

type TestOutEP = "sessions"
           :> Capture "sid" SessionId
           :> "test-out"
           :> PostNoContent '[JSON] ()

testOut :: Env -> SessionId -> Handler ()
testOut env sId = liftLib env sId testO
