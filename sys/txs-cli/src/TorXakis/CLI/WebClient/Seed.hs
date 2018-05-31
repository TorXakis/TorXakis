{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Set randomization seed in TorXakis.
module TorXakis.CLI.WebClient.Seed
(setSeed)
where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseStatus,
                                                statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Set randomization seed in TorXakis.
setSeed :: (MonadIO m, MonadReader Env m, MonadError String m)
        => String -> m ()
setSeed seedStr = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/seed"]
    r <- envPost path $ partString "seed" seedStr
    (r ^. responseStatus . statusCode) `shouldBeStatus` 204
