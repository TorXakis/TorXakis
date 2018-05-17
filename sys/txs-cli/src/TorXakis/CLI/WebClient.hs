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
    )
where

import           Control.Arrow          (right)
import           Control.Concurrent     (Chan, writeChan)
import           Control.Exception      (Handler (Handler), IOException,
                                         catches)
import           Control.Lens           ((^.), (^?))
import           Control.Lens.TH        (makeLenses)
import           Control.Monad          (when)
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         liftEither, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Aeson             (eitherDecode)
import           Data.Aeson.Lens        (key, _String)
import           Data.Aeson.Types       (toJSON)
import qualified Data.ByteString        as BSS
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString.Lazy   (ByteString)
import           Data.Either.Utils      (maybeToEither)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Client    (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException))
import           Network.Wreq           (Response, foldGet, get, partFile, post,
                                         put, responseBody, responseStatus,
                                         statusCode)
import           Network.Wreq.Types     (Postable, Putable)
import           System.FilePath        (takeFileName)
import           Text.Read              (readMaybe)

import           TorXakis.Lib           (StepType (AnAction, NumberOfSteps))

-- TODO: put the module below into 'TorXakis.WebServer.Endpoints.Parse' or something similar.
import           Endpoints.Parse        (ActionText (ActionText))

import           TorXakis.CLI.Env

data Info = Info
    { _version   :: Text
    , _buildTime :: Text
    } deriving (Eq, Show)
makeLenses ''Info

-- | Perform a get request on the given path, using the host specified in the
-- environment.
--
-- TODO: the path could be made type-safe, if the overhead is worth. Maybe we
-- can use an existing type-safe URL package.
envGet :: (MonadIO m, MonadReader Env m, MonadError String m)
       => String -> m (Response ByteString)
envGet suffix = do
    host <- asks txsHost
    res <- liftIO $ safe $
           get (host ++ suffix)
    liftEither res

envPut :: (MonadIO m, MonadReader Env m, MonadError String m, Putable a)
       => String -> a -> m (Response ByteString)
envPut suffix what = do
    host <- asks txsHost
    res <- liftIO $ safe $
        put (host ++ suffix) what
    liftEither res

envPost :: (MonadIO m, MonadReader Env m, MonadError String m, Postable a)
        => String -> a -> m (Response ByteString)
envPost suffix what = do
    host <- asks txsHost
    res <- liftIO $ safe $
        post (host ++ suffix) what
    liftEither res

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

-- | Retrieve the system information from 'TorXakis'.
getTime :: (MonadIO m, MonadReader Env m, MonadError String m)
        => m Text
getTime = do
    r <- envGet "time"
    (r ^. responseStatus . statusCode) `shouldBeStatus` 200
    liftEither $ maybeToEither "Response did not contain \"currentTime\"" $
                    r ^? responseBody . key "currentTime" . _String


-- | Retrieve the system information from 'TorXakis'.
-- callTimer :: (MonadIO m, MonadReader Env m, MonadError String m)
--         => m Text
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

shouldBeStatus :: (MonadError String m) => Int -> Int -> m ()
shouldBeStatus got expected =
    when (got /= expected) $
        throwError $ "Got status code " ++ show got
                   ++ " instead of " ++ show expected

-- | Load a list of files using the given environment.
--
load :: (MonadIO m, MonadReader Env m) => [FilePath] -> m (Either String ())
load files = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/model"]
    ignoreSuccess $ envPut path pfs
    where fns = map takeFileName files
          pfs  = zipWith partFile (map T.pack fns) files

safe :: IO a -> IO (Either String a)
safe io =
    fmap Right io `catches` [ Handler (return . Left . mapHttpException)
                            , Handler (return . Left . mapIOException)]

mapHttpException :: HttpException -> String
mapHttpException (HttpExceptionRequest _ (StatusCodeException _ msg)) = BS.unpack msg
mapHttpException e                          = show e -- TODO: give a more informative error.

mapIOException :: IOException -> String
mapIOException e = "IO exception: " ++ show e

noContent :: ByteString
noContent = ""

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
