{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Params command of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.Eval where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseBody,
                                                responseStatus, statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Evaluate an expression in TorXakis session.
evaluate :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m String
evaluate expr = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/eval"]
    r <- envPost path $ partString "expr" expr
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ BSL.unpack (r ^. responseBody)
