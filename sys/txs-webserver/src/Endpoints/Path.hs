{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.Path where

import           Servant

import           TorXakis.Lib               (pathDump, traceDump)

import           Common                     (Env, SessionId, liftLib)

type PathAPI = "sessions"
            :> Capture "sid" SessionId
            :>
            (
                "path" :> Get '[PlainText] String
            :<|>
                "trace"
                :>
                (
                    Get '[PlainText] String
                :<|>
                    Capture "fmt" String
                    :> Get '[PlainText] String
                )
            )

pathServer :: Env -> Server PathAPI
pathServer env sId = path :<|> (trace :<|> traceFmt)
    where
        path :: Handler String
        path = liftLib env sId pathDump
        trace :: Handler String
        trace = liftLib env sId (`traceDump` "")
        traceFmt :: String -> Handler String
        traceFmt fmt = liftLib env sId (`traceDump` fmt)
