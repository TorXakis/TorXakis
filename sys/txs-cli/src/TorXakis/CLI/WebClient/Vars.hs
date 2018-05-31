{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Create and fetch variables in a TorXakis session.
module TorXakis.CLI.WebClient.Vars
( createVar
, getVars
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
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseBody,
                                                responseStatus, statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Create a val in TorXakis session.
createVar :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m String
createVar varStr = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/vars"]
    r <- envPost path $ partString "t" varStr
    (r ^. responseStatus . statusCode) `shouldBeStatus` 201
    return $ "[ " ++ decodeVar (r ^. responseBody) ++ " ]"

-- | Retrieve all parameters
getVars :: (MonadIO m, MonadReader Env m, MonadError String m)
             => m String
getVars = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/vars"]
    r <- envGet path
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    let mVars = do
            results <- decode $ r ^. responseBody
            return $ mapMaybe (parseMaybe parseVarNm) results
    return $ case mVars of
        Nothing   -> "Invalid response: " ++ BSL.unpack (r ^. responseBody)
        Just vars -> "[ " ++ unwords vars ++ " ]"


decodeVar ::BSL.ByteString -> String
decodeVar respBody =
    let mVar = do
            result <- decode respBody
            parseMaybe parseVarNm result
    in  fromMaybe ("Invalid response: " ++ BSL.unpack respBody)
                  mVar

parseVarNm :: Object -> Parser String
parseVarNm o = o .: "_varName" :: Parser String
