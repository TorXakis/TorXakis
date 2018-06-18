{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Stepper where

import           Control.Monad.State     (lift)

import           Name                    (Name)
import qualified TxsCore                 as Core

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session

-- | Set the stepper.
setStep :: Session
        -> Name -- ^ Model name
        -> IO (Response ())
setStep s mn = runResponse $ do
    mDef <- lookupModel s mn
    lift $ runIOC s (Core.txsSetStep mDef)

-- | Step for n-steps or actions
step :: Session -> StepType -> IO (Response ())
step s (NumberOfSteps n) = runForVerdict s (Core.txsStepN n)
step s (AnAction a)      = runForVerdict s (Core.txsStepA a)

