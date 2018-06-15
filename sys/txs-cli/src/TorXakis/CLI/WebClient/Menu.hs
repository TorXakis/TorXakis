{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Params command of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.Menu where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseBody)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Get menu.
getMenu :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m String
getMenu args = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/menu"]
    r <- envPost path $ partString "args" args
    return $ BSL.unpack (r ^. responseBody)
