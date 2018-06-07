{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Common functionality of Web client for `txs-webserver`.
module TorXakis.CLI.WebClient.Common where

import           Control.Arrow          (right)
import           Control.Exception      (Handler (Handler), IOException,
                                         catches)
import           Control.Monad.Except   (ExceptT, MonadError, liftEither,
                                         runExceptT, throwError, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString.Lazy   (ByteString)
import           Network.HTTP.Client    (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException))
import           Network.Wreq           (Response, get, post, put)
import           Network.Wreq.Types     (Postable, Putable)

import           TorXakis.CLI.Env

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

shouldBeStatus :: (MonadError String m) => Int -> Int -> m ()
shouldBeStatus got expected =
    when (got /= expected) $
        throwError $ "Got status code " ++ show got
                   ++ " instead of " ++ show expected

ignoreSuccess :: (MonadIO m)
               => ExceptT String m (Response ByteString)
               -> m (Either String ())
ignoreSuccess = fmap (right (const ())) . runExceptT
