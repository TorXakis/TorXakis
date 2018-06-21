{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Set state in TorXakis.
module TorXakis.CLI.WebClient.States
( gotoState
, backState
) where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (responseBody, responseStatus,
                                                statusCode)

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

-- | Set state in TorXakis.
gotoState :: (MonadIO m, MonadReader Env m, MonadError String m)
        => Int -> m String
gotoState = changeState "goto"

-- | Step back in states in TorXakis.
backState :: (MonadIO m, MonadReader Env m, MonadError String m)
        => Int -> m String
backState = changeState "back"

changeState :: (MonadIO m, MonadReader Env m, MonadError String m)
            => String -> Int -> m String
changeState cmd st = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/state/", cmd, "/", show st]
    r <- envPost path noContent
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ BSL.unpack (r ^. responseBody)
