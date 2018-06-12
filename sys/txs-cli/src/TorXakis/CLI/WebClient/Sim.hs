{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Use tester in TorXakis.
module TorXakis.CLI.WebClient.Sim where

import           Control.Monad.Except          (liftEither)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Data.Aeson.Types              (toJSON)
import           Data.Either.Utils             (maybeToEither)
import           Network.Wreq                  (partString)
import           Text.Read                     (readMaybe)

import           TorXakis.Lib                  (StepType (NumberOfSteps))

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

startSimulator :: (MonadIO m, MonadReader Env m) => [String] -> m (Either String ())
startSimulator names = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/set-sim"])
                [ partString "model" $ head names
                , partString "cnect" $ last names
                , partString "map" $ unwords $ tail $ reverse $ tail names
                ]

simStep :: (MonadIO m, MonadReader Env m) => String -> m (Either String ())
simStep nStr = do
    sId <- asks sessionId
    ignoreSuccess $ do
        sType <- parseNumberOfSteps
        envPost (concat ["sessions/", show sId, "/sim/"]) (toJSON sType)
    where
      parseNumberOfSteps = liftEither $ maybeToEither "Expected a number" $
                                fmap NumberOfSteps (readMaybe nStr)
