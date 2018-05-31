{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | End-points for the parsing of TorXakis entities.
module Endpoints.Vars
    ( VarsAPI
    , varsServer
    )
where

import           Servant
import           Servant.Multipart      (Mem, MultipartData, MultipartForm,
                                         iValue, inputs)

import           Common       (Env, SessionId, liftLib)
import qualified TorXakis.Lib as Lib


type VarsAPI =  "sessions"
            :> Capture "sid" SessionId
            :> "vars"
            :>
            (
                MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] Lib.Var
            :<|>
                Get '[JSON] [Lib.Var]
            )


varsServer :: Env -> Server VarsAPI
varsServer env sId = createVar :<|> getVars
    where
        createVar :: MultipartData Mem -> Handler Lib.Var
        createVar mpData =
            case inputs mpData of
                (tIn:_rest) -> liftLib env sId (`Lib.createVar` iValue tIn)
                []          -> throwError err400 { errBody = "No input received" }
        getVars :: Handler [Lib.Var]
        getVars = liftLib env sId Lib.getVars

