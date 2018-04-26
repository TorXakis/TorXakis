{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Endpoints.Stepper
( StartStepperEP
, startStepper
, TakeNStepsEP
, takeNSteps
) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Text                  (Text)
import           Servant

import           TorXakis.Lib               (Response (..), StepType (..), step,
                                             stepper)

import           Common                     (Env, SessionId, getSession)

type StartStepperEP = "sessions"
                   :> Capture "sid" SessionId
                   :> "models"
                   :> Capture "model" Text
                   :> "stepper"
                   :> PostNoContent '[JSON] ()

startStepper :: Env -> SessionId -> Text -> Handler ()
startStepper env sid model = do
    s <- getSession env sid
    r <- liftIO $ do
            putStrLn $ "Starting stepper for model " ++ show model ++ " in session " ++ show sid
            r <- stepper s model
            print r
            return r
    case r of
        Success -> return ()
        Error m -> throwError $ err500 { errBody = pack m }

type TakeNStepsEP   = "sessions"
                   :> Capture "sid" SessionId
                   :> "stepper"
                   :> Capture "steps" Int
                   :> PostNoContent '[JSON] ()

takeNSteps :: Env -> SessionId -> Int -> Handler ()
takeNSteps env sid steps = do
    s <- getSession env sid
    r <- liftIO $ do
            putStrLn $ "Taking " ++ show steps ++ " steps in session " ++ show sid
            r <- step s $ NumberOfSteps steps
            print r
            return r
    case r of
        Success -> return ()
        Error m -> throwError $ err500 { errBody = pack m }

