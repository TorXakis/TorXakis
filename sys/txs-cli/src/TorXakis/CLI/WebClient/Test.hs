{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Use tester in TorXakis.
module TorXakis.CLI.WebClient.Test
( startTester
, testN
)
where

import           Control.Monad.Except          (catchError, liftEither,
                                                throwError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Data.Aeson                    (eitherDecode)
import           Data.Aeson.Types              (toJSON)
import           Data.Either.Utils             (maybeToEither)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Network.Wreq                  (responseBody)
import           Text.Read                     (readMaybe)

import           TorXakis.Lib                  (StepType (AnAction, NumberOfSteps))

import           Endpoints.Parse               (ActionText (ActionText))
import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common

startTester :: (MonadIO m, MonadReader Env m) => String -> String -> m (Either String ())
startTester mName cName = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/set-test/", mName, "/", cName]) noContent

testN :: (MonadIO m, MonadReader Env m) => String -> m (Either String ())
testN with = do
    sId <- asks sessionId
    ignoreSuccess $ do
        sType <- step1 `catchError` \_ ->
                 parseNumberOfSteps `catchError` \_ ->
                 parseAction sId
        envPost (concat ["sessions/", show sId, "/test/"]) (toJSON sType)
    where
      step1 = if null with
          then return $ NumberOfSteps 1
          else throwError "Non-empty argument to test"
      parseNumberOfSteps = liftEither $ maybeToEither "Expecting a number" $
          fmap NumberOfSteps (readMaybe with)
      parseAction sId = do
          let actText = ActionText (T.pack with)
          rsp <- envPost (concat ["sessions/", show sId, "/parse-action/"]) (toJSON actText)
          act <- liftEither $ eitherDecode $ rsp ^. responseBody
          return $ AnAction act
