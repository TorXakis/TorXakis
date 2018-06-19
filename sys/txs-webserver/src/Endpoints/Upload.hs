{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Upload
( UploadEP
, upload
) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON)
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           GHC.Generics               (Generic)
import           Servant
import           Servant.Multipart          (Mem, MultipartData, MultipartForm,
                                             fdFileName, fdPayload, files)

import           TorXakis.Lib               (load)

import           Common                     (Env, SessionId, getSession, checkResult)

type UploadEP = "sessions"
             :> Capture "sid" SessionId
             :> "model"
             :> MultipartForm Mem (MultipartData Mem)
             :> Put '[JSON] [FileLoadedResponse]

data FileLoadedResponse = FileLoadedResponse
                            { fileName :: Text
                            , loaded   :: Bool
                            }
    deriving (Generic)
instance ToJSON FileLoadedResponse

upload :: Env -> SessionId -> MultipartData Mem -> Handler [FileLoadedResponse]
upload env sid multipartData =  do
    s <- getSession env sid
    case files multipartData of
        [] -> throwError err400 { errBody = "No files received" }
        fs -> do
            r <- liftIO $ load s $ LT.unpack $ LT.concat $ map (decodeUtf8 . fdPayload) fs
            checkResult r
            let  mkResponse f = FileLoadedResponse { fileName = fdFileName f, loaded = True }
            return $ map mkResponse fs
