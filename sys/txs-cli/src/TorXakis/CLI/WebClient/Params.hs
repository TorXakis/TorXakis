{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Params command of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.Params
( getParam
, getAllParams
, setParam
)
where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Data.Aeson                    (decode)
import           Data.Aeson.Types              (Object, Parser, parseMaybe,
                                                (.:))
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Maybe                    (fromMaybe, mapMaybe)
-- import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (Response, partString,
                                                responseBody, responseStatus,
                                                statusCode)
-- import           Lens.Micro.Aeson              (key, _String)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Retrieve given parameter
getParam :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m String
getParam pNm = do
    r <- getParamsResponse pNm
    return $ decodeParam $ r ^. responseBody

-- | Retrieve all parameters
getAllParams :: (MonadIO m, MonadReader Env m, MonadError String m)
             => m String
getAllParams = do
    r <- getParamsResponse ""
    let mParams = do
            results <- decode $ r ^. responseBody
            return $ mapMaybe (parseMaybe parseParameterValue) results
    return $ case mParams of
        Nothing     -> "Invalid response: " ++ BSL.unpack (r ^. responseBody)
        Just params -> unlines params

-- | Set given parameter to given value
setParam :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> String -> m String
setParam nm vl = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/params"]
    r <- envPut path $ partString (T.pack nm) vl
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ decodeParam $ r ^. responseBody

getParamsResponse :: (MonadIO m, MonadReader Env m, MonadError String m)
                  => String -> m (Response BSL.ByteString)
getParamsResponse pNm = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/params/", pNm]
    r <- envGet path
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return r

decodeParam ::BSL.ByteString -> String
decodeParam respBody =
    let mParam = do
            result <- decode respBody
            parseMaybe parseParameterValue result
    in  fromMaybe ("Invalid response: " ++ BSL.unpack respBody)
                  mParam

parseParameterValue :: Object -> Parser String
parseParameterValue o = do
    n <- o .: "_paramName"  :: Parser String
    v <- o .: "_paramValue" :: Parser String
    return $ concat [n, " = ", v]
