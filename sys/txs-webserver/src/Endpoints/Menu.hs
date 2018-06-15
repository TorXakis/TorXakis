{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
-- | End-point for evaulating an expression.
module Endpoints.Menu
    ( MenuEP
    , menu
    )
where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Servant
import           Servant.Multipart          (Input (Input), Mem, MultipartData,
                                             MultipartForm, inputs)

import           Common                     (Env, SessionId, getSession)
import qualified TorXakis.Lib               as Lib

type MenuEP = "sessions"
            :> Capture "sid" SessionId
            :> "menu"
            :> MultipartForm Mem (MultipartData Mem)
            :> Post '[PlainText] String

menu :: Env -> SessionId -> MultipartData Mem -> Handler String
menu env sId mpData = do
    s <- getSession env sId
    case inputs mpData of
        [Input "args" args] -> liftIO $ Lib.getMenu s args
        []                  -> liftIO $ Lib.getMenu s ""
        is                  -> throwError err400 { errBody = BSL.pack $ "Too many inputs: " ++ show is }
