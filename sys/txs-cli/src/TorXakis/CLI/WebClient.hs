{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- | Web client for `txs-webserver`.
module TorXakis.CLI.WebClient
    ( module TorXakis.CLI.WebClient
    , module TorXakis.CLI.WebClient.Eval
    , module TorXakis.CLI.WebClient.LPE
    , module TorXakis.CLI.WebClient.Menu
    , module TorXakis.CLI.WebClient.NComp
    , module TorXakis.CLI.WebClient.Params
    , module TorXakis.CLI.WebClient.Seed
    , module TorXakis.CLI.WebClient.Sim
    , module TorXakis.CLI.WebClient.Solve
    , module TorXakis.CLI.WebClient.States
    , module TorXakis.CLI.WebClient.Test
    , module TorXakis.CLI.WebClient.Vals
    , module TorXakis.CLI.WebClient.Vars
    )
where

import           Control.Concurrent.STM.TChan  (TChan, writeTChan)
import           Control.Concurrent.STM.TVar   (writeTVar)
import           Control.Monad.Except          (MonadError, catchError,
                                                liftEither, throwError)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, asks)
import           Control.Monad.STM             (atomically)
import           Data.Aeson                    (eitherDecode)
import           Data.Aeson.Types              (toJSON)
import qualified Data.ByteString               as BSS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Either.Utils             (maybeToEither)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.), (^?))
import           Lens.Micro.Aeson              (key, _String)
import           Lens.Micro.TH                 (makeLenses)
import           Network.Wreq                  (foldGet, partFile, partString,
                                                responseBody, responseStatus,
                                                statusCode)
import           System.FilePath               (takeFileName)
import           Text.Read                     (readMaybe)

import           TorXakis.Lib                  (StepType (AnAction, NumberOfSteps))

-- TODO: put the module below into 'TorXakis.WebServer.Endpoints.Parse' or something similar.
import           Endpoints.Parse               (ActionText (ActionText))

import           TorXakis.CLI.Env
import qualified TorXakis.CLI.Log              as Log
import           TorXakis.CLI.WebClient.Common
import           TorXakis.CLI.WebClient.Eval
import           TorXakis.CLI.WebClient.LPE
import           TorXakis.CLI.WebClient.Menu
import           TorXakis.CLI.WebClient.NComp
import           TorXakis.CLI.WebClient.Params
import           TorXakis.CLI.WebClient.Seed
import           TorXakis.CLI.WebClient.Sim
import           TorXakis.CLI.WebClient.Solve
import           TorXakis.CLI.WebClient.States
import           TorXakis.CLI.WebClient.Test
import           TorXakis.CLI.WebClient.Vals
import           TorXakis.CLI.WebClient.Vars

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
          => String -> m Text
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
            then return $ T.concat ["Timer ", tnm, " started at ", stt, "."]
            else return $ T.concat ["Timer ", tnm, " stopped at ", stp, ". Duration: ", d]

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
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/set-step/", mName]) noContent

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

stopTxs :: (MonadIO m, MonadReader Env m) => m (Either String ())
stopTxs = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/stop"]) [partString "" ""]

-- | Open the messages endpoint
openMessages :: (MonadIO m, MonadReader Env m) => m (Either String ())
openMessages = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/messages/open/"]) noContent

sseSubscribe :: Env -> TChan BSS.ByteString -> String -> IO ()
sseSubscribe env ch suffix = do
    Log.info "Subscribing to messages..."
    res <- safe (foldGet act () (host ++ suffix))
    Log.info ("Exited foldGet that subscribed to messages, with: " ++ show res)
    where
      host = txsHost env
      act _ = atomically . writeTChan ch

-- | Close the messages endpoint
closeMessages :: (MonadIO m, MonadReader Env m) => m (Either String ())
closeMessages = do
    sId <- asks sessionId
    ignoreSuccess $
        envPost (concat ["sessions/", show sId, "/messages/close/"]) noContent

-- | Show an entity in TorXakis session
showTxs :: (MonadIO m, MonadReader Env m, MonadError String m)
         => [String] -> m String
showTxs args = do
    sId <- asks sessionId
    r <- case args of
            [item]     -> envGet $ concat ["sessions/", show sId, "/show/", item]
            [item, nm] -> envGet $ concat ["sessions/", show sId, "/show/", item, "/", nm]
            _          -> throwError "Usage: show <item> [<name>]"
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ BSL.unpack (r ^. responseBody)

-- | Retrieve path to current state
getPath :: (MonadIO m, MonadReader Env m, MonadError String m)
          => m String
getPath = do
    sId <- asks sessionId
    r <- envGet $ concat ["sessions/", show sId, "/path"]
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ BSL.unpack (r ^. responseBody)

-- | Retrieve [formatted] trace to current state
getTrace :: (MonadIO m, MonadReader Env m, MonadError String m)
          => String -> m String
getTrace fmt = do
    sId <- asks sessionId
    r <- envGet $ concat ["sessions/", show sId, "/trace/", fmt]
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    return $ BSL.unpack (r ^. responseBody)
