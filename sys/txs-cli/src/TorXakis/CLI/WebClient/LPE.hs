{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Params command of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.LPE where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseStatus,
                                                statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Run LPE for a model or process in current TorXakis session.
lpe :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m ()
lpe args = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/lpe"]
    r <- envPost path $ partString "args" args
    (r ^. responseStatus . statusCode) `shouldBeStatus` 202
    return ()
