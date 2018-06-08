{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | End-points for the parsing of TorXakis entities.
module Endpoints.Vals
    ( ValsAPI
    , valsServer
    )
where

import           Servant
import           Servant.Multipart (Mem, MultipartData, MultipartForm, iValue,
                                    inputs)

import           Common            (Env, SessionId, liftLib)
import qualified TorXakis.Lib      as Lib


type ValsAPI =  "sessions"
            :> Capture "sid" SessionId
            :> "vals"
            :>
            (
                MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] Lib.Val
            :<|>
                Get '[JSON] [Lib.Val]
            )


valsServer :: Env -> Server ValsAPI
valsServer env sId = createVal :<|> getVals
    where
        createVal :: MultipartData Mem -> Handler Lib.Val
        createVal mpData =
            case inputs mpData of
                (tIn:_rest) -> liftLib env sId (`Lib.createVal` iValue tIn)
                []          -> throwError err400 { errBody = "No input received" }
        getVals :: Handler [Lib.Val]
        getVals = liftLib env sId Lib.getVals

