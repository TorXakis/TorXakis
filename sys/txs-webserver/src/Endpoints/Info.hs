{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Endpoints.Info
    ( InfoEP
    , getInfo
    ) where

import           Servant

import           TorXakis.Lib     (info, TorXakisInfo)

type InfoEP = "info" :> Get '[JSON] TorXakisInfo

getInfo :: Handler TorXakisInfo
getInfo = return TorXakis.Lib.info
