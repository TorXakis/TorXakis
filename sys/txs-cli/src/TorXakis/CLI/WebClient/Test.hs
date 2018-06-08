{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Use tester in TorXakis.
module TorXakis.CLI.WebClient.Test where

import           Control.Monad.Except          (catchError, liftEither)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Data.Aeson                    (eitherDecode)
import           Data.Aeson.Types              (toJSON)
import           Data.Either.Utils             (maybeToEither)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (partString, responseBody)
import           Text.Read                     (readMaybe)

import           TorXakis.Lib                  (StepType (AnAction, NumberOfSteps))

import           Endpoints.Parse               (ActionText (ActionText))
import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

startTester :: (MonadIO m, MonadReader Env m) => [String] -> m (Either String ())
startTester names = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/set-test"])
                [ partString "model" $ head names
                , partString "cnect" $ last names
                , partString "purp&map" $ unwords $ tail $ reverse $ tail names
                ]

testStep :: (MonadIO m, MonadReader Env m) => String -> m (Either String ())
testStep with = do
    sId <- asks sessionId
    ignoreSuccess $
        if null with
            then envPost (concat ["sessions/", show sId, "/test-out"]) noContent
            else do
                sType <- parseNumberOfSteps `catchError` \_ ->
                        parseAction sId
                envPost (concat ["sessions/", show sId, "/test/"]) (toJSON sType)
    where
      parseNumberOfSteps = liftEither $ maybeToEither "Expecting a number" $
          fmap NumberOfSteps (readMaybe with)
      parseAction sId = do
          let actText = ActionText (T.pack with)
          rsp <- envPost (concat ["sessions/", show sId, "/parse-action/"]) (toJSON actText)
          act <- liftEither $ eitherDecode $ rsp ^. responseBody
          return $ AnAction act
