
{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Tester where

import           Servant
import           Servant.Multipart (Mem, MultipartData, MultipartForm, iName,
                                    iValue, inputs)

import           TorXakis.Lib      (StepType (..), setTest, test, testO)

import           Common            (Env, SessionId, liftLib)

type SetTestEP = "sessions"
              :> Capture "sid" SessionId
              :> "set-test"
              :> MultipartForm Mem (MultipartData Mem)
              :> PostNoContent '[JSON] ()

startTester :: Env -> SessionId -> MultipartData Mem -> Handler ()
startTester env sId mpData = do
    let is = inputs mpData
    if length is /= 3
        then throwError err400 { errBody = "Invalid arguments to set-test." }
        else do
            let (m,c,pm) = foldr extractName ("","","") is
            liftLib env sId $ setTest m c pm
  where
    extractName inp (m,c,pm) =
        case iName inp of
            "model"    -> (iValue inp, c, pm)
            "cnect"    -> (m, iValue inp, pm)
            "purp&map" -> (m, c, iValue inp)
            _          -> (m, c, pm)

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
