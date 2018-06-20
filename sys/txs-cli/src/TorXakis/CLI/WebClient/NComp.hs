{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module TorXakis.CLI.WebClient.NComp where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (responseStatus, statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Generate test purpose with N-Complete algorithm.
ncomp :: (MonadIO m, MonadReader Env m, MonadError String m)
         => String -> m ()
ncomp modelName = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/ncomp/", modelName]
    r <- envPost path noContent
    (r ^. responseStatus . statusCode) `shouldBeStatus` 202
    return ()
