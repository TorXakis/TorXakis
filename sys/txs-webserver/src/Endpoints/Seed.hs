{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | End-point for setting random seed.
module Endpoints.Seed
    ( SeedEP
    , setSeed
    )
where

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Semigroup             ((<>))
import           Data.Text                  (unpack)
import           Servant
import           Servant.Multipart          (Input (Input), Mem, MultipartData,
                                             MultipartForm, inputs)
import           Text.Read                  (readMaybe)

import           Common                     (Env, SessionId, getSession)
import qualified TorXakis.Lib               as Lib

type SeedEP = "sessions"
            :> Capture "sid" SessionId
            :> "seed"
            :> MultipartForm Mem (MultipartData Mem)
            :> PostNoContent '[JSON] ()

setSeed :: Env -> SessionId -> MultipartData Mem -> Handler ()
setSeed env sId mpData =
    case inputs mpData of
        [Input "seed" seedTxt] ->
            case readMaybe $ unpack seedTxt of
                Nothing   -> throwError err400 { errBody = BSL.pack $ unpack $ "Can't parse input: " <> seedTxt }
                Just seed -> do
                    s <- getSession env sId
                    liftIO $ Lib.setSeed s seed
        []                  -> throwError err400 { errBody = "No input received" }
        is                  -> throwError err400 { errBody = BSL.pack $ "Too many inputs: " ++ show is }
