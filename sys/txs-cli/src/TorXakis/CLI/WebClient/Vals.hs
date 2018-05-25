{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Params command of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.Vals
( createVal
, getVals
)
where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseBody,
                                                responseStatus, statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Create a val in TorXakis session.
createVal :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m String
createVal valStr = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/vals"]
    r <- envPost path $ partString "t" valStr
    (r ^. responseStatus . statusCode) `shouldBeStatus` 201
    return $ BSL.unpack $ r ^. responseBody

-- | Retrieve all parameters
getVals :: (MonadIO m, MonadReader Env m, MonadError String m)
             => m String
getVals = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/vals"]
    r <- envGet path
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ BSL.unpack $ r ^. responseBody
