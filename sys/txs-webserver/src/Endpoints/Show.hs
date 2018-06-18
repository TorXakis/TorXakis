{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
-- | End-point for showing a defined entity.
module Endpoints.Show where

import           Control.Monad.IO.Class (liftIO)
import           Servant

import           Common                 (Env, SessionId, getSession)
import qualified TorXakis.Lib           as Lib

type ShowAPI = "sessions"
            :> Capture "sId" SessionId
            :> "show"
            :> Capture "itm" String
            :>
            (
                Capture "nm" String :> Get '[PlainText] String
            :<|>
                Get '[PlainText] String
            )

showServer :: Env -> Server ShowAPI
showServer env sId itm = showAll :<|> showNamed
    where
        showAll :: String -> Handler String
        showAll nm = do
            s <- getSession env sId
            liftIO $ Lib.showItem s itm nm

        showNamed :: Handler String
        showNamed = do
            s <- getSession env sId
            liftIO $ Lib.showItem s itm ""
