{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Endpoints.Time
    ( TimeEP
    , getTime
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Servant

import           TorXakis.Lib           (TimeResult, time)

type TimeEP = "time" :> Get '[JSON] TimeResult

getTime :: Handler TimeResult
getTime = liftIO time
