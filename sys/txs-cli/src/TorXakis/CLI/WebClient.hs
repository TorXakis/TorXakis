{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- | Web client for `txs-webserver`.
module TorXakis.CLI.WebClient
    ( info
    , Info
    , getTime
    , version
    , buildTime
    , load
    , stepper
    , step
    , openMessages
    , sseSubscribe
    , closeMessages
    , callTimer
    , module TorXakis.CLI.WebClient.Params
    )
where

import           Control.Arrow                 (right)
import           Control.Concurrent            (Chan, writeChan)
import           Control.Monad.Except          (ExceptT, MonadError, catchError,
                                                liftEither, runExceptT,
                                                throwError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Data.Aeson                    (eitherDecode)
import           Data.Aeson.Types              (toJSON)
import qualified Data.ByteString               as BSS
import           Data.ByteString.Lazy          (ByteString)
import           Data.Either.Utils             (maybeToEither)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.), (^?))
import           Lens.Micro.Aeson              (key, _String)
import           Lens.Micro.TH                 (makeLenses)
import           Network.Wreq                  (Response, foldGet, partFile,
                                                responseBody, responseStatus,
                                                statusCode)
import           System.FilePath               (takeFileName)
import           Text.Read                     (readMaybe)

import           TorXakis.Lib                  (StepType (AnAction, NumberOfSteps))

-- TODO: put the module below into 'TorXakis.WebServer.Endpoints.Parse' or something similar.
import           Endpoints.Parse               (ActionText (ActionText))

import           TorXakis.CLI.Env
import           TorXakis.CLI.WebClient.Common
import           TorXakis.CLI.WebClient.Params

data Info = Info
    { _version   :: Text
    , _buildTime :: Text
    } deriving (Eq, Show)
makeLenses ''Info

-- | Retrieve the system information from 'TorXakis'.
info :: (MonadIO m, MonadReader Env m, MonadError String m)
     => m Info
info = do
    r <- envGet "info"
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    liftEither $ do
        v <- maybeToEither "Response did not contain \"version\"" $
             r ^? responseBody . key "version" . _String
        b <- maybeToEither "Response did not contain \"buildTime\"" $
             r ^? responseBody . key "buildTime" . _String
        return $ Info v b

-- | Retrieve the system time from 'TorXakis'.
getTime :: (MonadIO m, MonadReader Env m, MonadError String m)
        => m Text
getTime = do
    r <- envGet "time"
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    liftEither $ maybeToEither "Response did not contain \"currentTime\"" $
                    r ^? responseBody . key "currentTime" . _String

-- | Start/stop a timer in 'TorXakis'.
callTimer :: (MonadIO m, MonadReader Env m, MonadError String m)
          => String -> m String
callTimer nm = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/timers/", nm]
    r <- envPost path noContent
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    liftEither $ do
        tnm <- maybeToEither "Response did not contain \"timerName\"" $
                    r ^? responseBody . key "timerName" . _String
        stt <- maybeToEither "Response did not contain \"startTime\"" $
                    r ^? responseBody . key "startTime" . _String
        stp <- maybeToEither "Response did not contain \"stopTime\"" $
                    r ^? responseBody . key "stopTime" . _String
        d   <- maybeToEither "Response did not contain \"duration\"" $
                    r ^? responseBody . key "duration" . _String
        if T.null d
            then return $ concat ["Timer ", T.unpack tnm, " started at " ++ T.unpack stt ++ "."]
            else return $ concat ["Timer ", T.unpack tnm, " stopped at " ++ T.unpack stp ++ ". Duration: ", T.unpack d]

-- | Load a list of files using the given environment.
--
load :: (MonadIO m, MonadReader Env m) => [FilePath] -> m (Either String ())
load files = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/model"]
    ignoreSuccess $ envPut path pfs
    where fns = map takeFileName files
          pfs  = zipWith partFile (map T.pack fns) files

stepper :: (MonadIO m, MonadReader Env m) => String -> m (Either String ())
stepper mName = do
    sId <- asks sessionId
    ignoreSuccess $ do
        _ <- envPost (concat ["sessions/", show sId, "/set-step/", mName]) noContent
        envPost (concat ["sessions/", show sId, "/start-step/"]) noContent


step :: (MonadIO m, MonadReader Env m) => String -> m (Either String ())
step with = do
    sId <- asks sessionId
    ignoreSuccess $ do
        sType <- step1 `catchError` \_ ->
                 parseNumberOfSteps `catchError` \_ ->
                 parseAction sId
        envPost (concat ["sessions/", show sId, "/step/"]) (toJSON sType)
    where
      step1 = if null with
          then return $ NumberOfSteps 1
          else throwError "Non-empty argument to step"
      parseNumberOfSteps = liftEither $ maybeToEither "Expecting a number" $
          fmap NumberOfSteps (readMaybe with)
      parseAction sId = do
          let actText = ActionText (T.pack with)
          rsp <- envPost (concat ["sessions/", show sId, "/parse-action/"]) (toJSON actText)
          act <- liftEither $ eitherDecode $ rsp ^. responseBody
          return $ AnAction act

ignoreSuccess :: (MonadIO m)
               => ExceptT String m (Response ByteString)
               -> m (Either String ())
ignoreSuccess = fmap (right (const ())) . runExceptT

-- | Open the messages endpoint
openMessages :: (MonadIO m, MonadReader Env m) => m (Either String ())
openMessages = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/messages/open/"]) noContent

sseSubscribe :: Env -> Chan BSS.ByteString -> String -> IO (Either String ())
sseSubscribe env ch suffix =
    safe $ foldGet act () (host ++ suffix)
    where
      host = txsHost env
      act _ = writeChan ch

-- | Close the messages endpoint
closeMessages :: (MonadIO m, MonadReader Env m) => m (Either String ())
closeMessages = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/messages/close/"]) noContent
