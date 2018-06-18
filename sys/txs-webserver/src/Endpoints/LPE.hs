{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
-- | End-point for triggering LPE.
module Endpoints.LPE
    ( LpeEP
    , lpe
    )
where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Servant
import           Servant.Multipart          (Input (Input), Mem, MultipartData,
                                             MultipartForm, inputs)

import           Common                     (Env, SessionId, liftLib)
import qualified TorXakis.Lib               as Lib

type LpeEP = "sessions"
            :> Capture "sid" SessionId
            :> "lpe"
            :> MultipartForm Mem (MultipartData Mem)
            :> PostAccepted '[JSON] ()

lpe :: Env -> SessionId -> MultipartData Mem -> Handler ()
lpe env sId mpData =
    case inputs mpData of
        [Input "args" args] -> liftLib env sId (`Lib.lpe` args)
        []                  -> throwError err400 { errBody = "No input received" }
        is                  -> throwError err400 { errBody = BSL.pack $ "Too many inputs: " ++ show is }
