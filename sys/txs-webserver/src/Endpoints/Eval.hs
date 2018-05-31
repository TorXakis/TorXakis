{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
-- | End-point for evaulating an expression.
module Endpoints.Eval
    ( EvalEP
    , eval
    )
where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Servant
import           Servant.Multipart          (Input (Input), Mem, MultipartData,
                                             MultipartForm, inputs)

import           Common                     (Env, SessionId, liftLib)
import qualified TorXakis.Lib               as Lib

type EvalEP = "sessions"
            :> Capture "sid" SessionId
            :> "eval"
            :> MultipartForm Mem (MultipartData Mem)
            :> Post '[PlainText] String

eval :: Env -> SessionId -> MultipartData Mem -> Handler String
eval env sId mpData =
    case inputs mpData of
        [Input "expr" expr] -> liftLib env sId (`Lib.eval` expr)
        []                  -> throwError err400 { errBody = "No input received" }
        is                  -> throwError err400 { errBody = BSL.pack $ "Too many inputs: " ++ show is }
