{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | End-point for solving an expression.
module Endpoints.Solve where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Servant
import           Servant.Multipart          (Input (Input), Mem, MultipartData,
                                             MultipartForm, inputs)

import           Common                     (Env, SessionId, liftLib)
import qualified TorXakis.Lib               as Lib

type SolveEP = "sessions"
            :> Capture "sid" SessionId
            :> "solve"
            :> Capture "kind" String
            :> MultipartForm Mem (MultipartData Mem)
            :> Post '[PlainText] String

solve :: Env -> SessionId -> String -> MultipartData Mem -> Handler String
solve env sId kind mpData = do
    act <- case kind of
        "sol" -> return Lib.solve
        "uni" -> return Lib.unisolve
        "ran" -> return Lib.ransolve
        _     -> throwError err400 { errBody = BSL.pack $ "Unknown solver: " ++ kind }
    case inputs mpData of
        [Input "expr" expr] -> liftLib env sId (`act` expr)
        []                  -> throwError err400 { errBody = "No input received" }
        is                  -> throwError err400 { errBody = BSL.pack $ "Too many inputs: " ++ show is }
