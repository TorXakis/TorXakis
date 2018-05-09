{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Endpoints.Stepper
    ( SetStepperEP
    , StartStepperEP
    , setStep
    , startStep
    , TakeNStepsEP
    , takeNSteps
    )
where

import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Text                  (Text)
import           Servant

import           TorXakis.Lib               (Response (..), StepType (..), step)
import qualified TorXakis.Lib               as Lib

import           Common                     (Env, SessionId, getSession)

type SetStepperEP = "sessions"
                   :> Capture "sid" SessionId
                   :> "set-step"
                   :> Capture "model" Text
                   :> PostNoContent '[JSON] ()

setStep :: Env -> SessionId -> Text -> Handler ()
setStep env sid model = do
    s <- getSession env sid
    r <- liftIO $ Lib.setStep s model
    case r of
        Success -> return ()
        Error m -> throwError $ err500 { errBody = pack m }

type StartStepperEP = "sessions"
                   :> Capture "sid" SessionId
                   :> "start-step"
                   :> PostNoContent '[JSON] ()

startStep :: Env -> SessionId -> Handler ()
startStep env sid = do
    s <- getSession env sid
    r <- liftIO $ Lib.startStep s
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
    r <- liftIO $ step s $ NumberOfSteps steps
    case r of
        Success -> return ()
        Error m -> throwError $ err500 { errBody = pack m }

