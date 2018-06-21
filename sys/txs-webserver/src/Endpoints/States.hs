{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.States where

import           Control.Monad.State        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Servant

import           TorXakis.Lib               (backStates, gotoState, showItem)

import           Common                     (Env, SessionId, getSession)

type StatesEP = "sessions"
           :> Capture "sid" SessionId
           :> "state"
           :> Capture "cmd" String
           :> Capture "st" Int
           :> Post '[PlainText] String

states :: Env -> SessionId -> String -> Int -> Handler String
states env sId cmd st = do
    s <- getSession env sId
    _ <- case cmd of
        "goto" -> liftIO $ gotoState s st
        "back" -> liftIO $ backStates s st
        _      -> throwError err400 { errBody = BSL.pack $ "Unknown command for state: " ++ cmd }
    liftIO $ do
        curSt <- showItem s "state" "nr"
        return $ "Current state set to: " ++ curSt
