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
import           Data.ByteString.Lazy.Char8  (pack)
import           Data.Text                   (Text)
import           Servant

import           TorXakis.Lib                (tester, Response (..), test, StepType (..))

import           Common (SessionId, getSession, Env)

type StartTesterEP  = "tester"
                   :> "start"
                   :> Capture "sid" SessionId
                   :> Capture "model" Text
                   :> Post '[JSON] String
                      
type TestNStepsEP   = "tester"
                   :> "test"
                   :> Capture "sid" SessionId
                   :> Capture "steps" Int
                   :> Post '[JSON] String

startTester :: Env -> SessionId -> Text -> Handler String
startTester env sid model = do
    s <- getSession env sid
    r <- liftIO $ do
            putStrLn $ "Starting tester for model " ++ show model ++ " in session " ++ show sid 
            r <- tester s model
            print r
            return r
    case r of
        Success -> return $ show Success
        Error m -> throwError $ err500 { errBody = pack m }

testNSteps :: Env -> SessionId -> Int -> Handler String
testNSteps env sid steps = do
    s <- getSession env sid
    r <- liftIO $ do
            putStrLn $ "Taking " ++ show steps ++ " test steps in session " ++ show sid 
            r <- test s $ NumberOfSteps steps
            print r
            return r
    case r of
        Success -> return $ show Success
        Error m -> throwError $ err500 { errBody = pack m }

