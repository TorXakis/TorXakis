{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Endpoints.Upload
( UploadEP
, upload
) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           GHC.Generics               (Generic)
import           Servant
import           Servant.Multipart          (Mem, MultipartData, MultipartForm,
                                             fdFileName, fdPayload, files)

import           TorXakis.Lib               (Response (..), load)
-- import           TorXakis.Lib.Session       (Session)

import           Common                     (Env, SessionId, getSession)

type UploadEP = "sessions"
             :> Capture "sid" SessionId
             :> "model"
             :> MultipartForm Mem (MultipartData Mem)
             :> PutAccepted '[JSON] [FileLoadedResponse]

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
            case r of
                Success -> return $ map mkResponse fs
                Error e -> throwError err400 { errBody = pack e }
              where
                mkResponse f = FileLoadedResponse { fileName=fdFileName f, loaded=True }
