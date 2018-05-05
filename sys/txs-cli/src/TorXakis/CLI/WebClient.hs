{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- | Web client for `txs-webserver`.
module TorXakis.CLI.WebClient
    ( info
    , Info
    , version
    , buildTime
    , load
    , stepper
    , step
    )
where

import           Data.Text              (Text)
--import           Data.Aeson             (FromJSON, decode)
import           Data.Aeson.Lens        (key, _String)
import           Data.Either.Utils      (maybeToEither)
--import           Network.Wreq
import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Data.List                  (intercalate)
-- import           Data.Text                  (Text)
-- import           GHC.Generics               (Generic)
import           Control.Arrow          (right)
import           Control.Exception      (Handler (Handler), IOException,
                                         catches)
import           Control.Lens           ((^.), (^?))
import           Control.Lens.TH        (makeLenses)
import           Control.Monad          (when)
import           Control.Monad.Except   (ExceptT, MonadError, liftEither,
                                         runExceptT, throwError)
import           Control.Monad.Reader   (MonadReader, asks)
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Text              as T
import           Network.HTTP.Client    (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException))
import           Network.Wreq           (Response, get, partFile, post, put,
                                         responseBody, responseStatus,
                                         statusCode)
import           Network.Wreq.Types     (Postable, Putable)
import           System.FilePath        (takeFileName)

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

shouldBeStatus :: (MonadError String m) => Int -> Int -> m ()
shouldBeStatus got expected =
    when (got /= expected) $
        throwError $ "Got status code " ++ show got
                   ++ " instead of " ++ show expected

-- | Load a list of files using the given environment.
load :: (MonadIO m, MonadReader Env m) => [FilePath] -> m (Either String ())
load files = do
    sId <- asks sessionId
    let path = concat ["sessions/", show sId, "/model"]
    ignoreResponse $ envPut path pfs
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
    ignoreResponse $
        envPost (concat ["sessions/", show sId, "/models/", mName, "/stepper"]) noContent

step :: (MonadIO m, MonadReader Env m) => Int -> m (Either String ())
step n = do
    sId <- asks sessionId
    ignoreResponse $
        envPost (concat ["sessions/", show sId, "/stepper/", show n]) noContent

ignoreResponse :: (MonadIO m)
               => ExceptT String m (Response ByteString)
               -> m (Either String ())
ignoreResponse = fmap (right (const ())) . runExceptT

