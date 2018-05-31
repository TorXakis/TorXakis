{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.Stepper
    ( SetStepperEP
    , setStep
    , StepEP
    , step
    )
where

import           Data.Text    (Text)
import           Servant

import           TorXakis.Lib (StepType (..))
import qualified TorXakis.Lib as Lib

import           Common       (Env, SessionId, liftLib)

type SetStepperEP = "sessions"
                   :> Capture "sid" SessionId
                   :> "set-step"
                   :> Capture "model" Text
                   :> PostNoContent '[JSON] ()

setStep :: Env -> SessionId -> Text -> Handler ()
setStep env sId model = liftLib env sId (`Lib.setStep` model)

type StepEP   = "sessions"
                :> Capture "sid" SessionId
                :> "step"
                :> ReqBody '[JSON] StepType
                :> PostNoContent '[JSON] ()

step :: Env -> SessionId -> StepType -> Handler ()
step env sId sType = liftLib env sId (`Lib.step` sType)
