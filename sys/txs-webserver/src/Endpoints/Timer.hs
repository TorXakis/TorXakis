{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.Timer
    ( TimerEP
    , timer
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Servant

import qualified TorXakis.Lib           as Lib

import           Common                 (Env, SessionId, getSession)

type TimerEP = "sessions"
             :> Capture "sid" SessionId
             :> "timers"
             :> Capture "nm"  String
             :> Post '[JSON] Lib.Timer

timer :: Env -> SessionId -> String -> Handler Lib.Timer
timer env sid nm = do
    s <- getSession env sid
    liftIO $ Lib.timer s nm
