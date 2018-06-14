{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.Stop
    ( StopEP
    , stop
    ) where

import           Servant

import           TorXakis.Lib           (stopTxs)

import           Common            (Env, SessionId, liftLib)

type StopEP = "sessions"
           :> Capture "sid" SessionId
           :> "stop"
           :> PostNoContent '[JSON] ()

stop :: Env -> SessionId -> Handler ()
stop env sId = liftLib env sId stopTxs
