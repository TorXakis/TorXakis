{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- | End-point for triggering LPE.
module Endpoints.NComp
    ( NCompEP
    , ncomp
    )
where

import           Data.Text                  (Text)
import           Servant

import           Common                     (Env, SessionId, liftLib)
import qualified TorXakis.Lib               as Lib

type NCompEP = "sessions"
            :> Capture "sid" SessionId
            :> "ncomp"
            :> Capture "modelName" Text
            :> PostAccepted '[JSON] ()

ncomp :: Env -> SessionId -> Text -> Handler ()
ncomp env sId modelName = liftLib env sId (`Lib.ncomp` modelName)
