{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Endpoints.Upload
( UploadEP
, upload
) where

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Foldable
import           Servant
import           Servant.Multipart           (MultipartForm, MultipartData, Mem, iName, iValue, inputs, files, fdPayload, fdFileName)

import           Common (SessionId, TxsHandler)

type UploadEP = "session" :> Capture "sid" SessionId :> "model" :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] Integer

upload :: SessionId -> MultipartData Mem -> TxsHandler Integer
upload _sid multipartData =  do
    liftIO $ do
        putStrLn "Inputs:"
        forM_ (inputs multipartData) $ \input ->
            putStrLn $ "  " ++ show (iName input)
                    ++ " -> " ++ show (iValue input)
    
        forM_ (files multipartData) $ \file -> do
            let content = fdPayload file
            putStrLn $ "Content of " ++ show (fdFileName file)
            LBS.putStr content
    return 0
