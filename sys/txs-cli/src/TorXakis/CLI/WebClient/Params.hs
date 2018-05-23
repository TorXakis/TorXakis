{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Params command of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.Params where

import           Control.Monad.Except          (MonadError, throwError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Data.Aeson                    (decode)
import           Data.Aeson.Types              (Object, Parser, parseMaybe,
                                                (.:))
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Maybe                    (mapMaybe)
-- import           Data.Text                     (Text)
-- import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (responseBody, responseStatus,
                                                statusCode)
-- import           Lens.Micro.Aeson              (key, _String)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common


-- | Retrieve parameters from 'TorXakis' web server.
getParams :: (MonadIO m, MonadReader Env m, MonadError String m)
        => [String] -> m String
getParams []  = getParam ""
getParams [p] = getParam p
getParams e   = throwError $ "One param at a time: " ++ show e

getParam :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m String
getParam pNm = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/params/", pNm]
    r <- envGet path
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    let mParams = do
            results <- decode $ r ^. responseBody
            return $ mapMaybe (parseMaybe parseParameterValue) results
    return $ case mParams of
        Nothing     -> "Invalid response: " ++ BSL.unpack (r ^. responseBody)
        Just params -> unlines params
    where
        parseParameterValue :: Object -> Parser String
        parseParameterValue o = do
            n <- o .: "_paramName"  :: Parser String
            v <- o .: "_paramValue" :: Parser String
            return $ concat [n, " = ", v]

