{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module CLI
(
  startCLI
, Env (..)
, TorXakisServerException (..)
)
where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async, cancel)
import           Control.Exception.Safe     (Exception)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ReaderT, asks)
import           Control.Monad.Trans        (lift)
import           Data.Aeson                 (decode)
import           Data.Aeson.Types           (Object, Parser, parseMaybe, (.:))
import           Data.Char                  (toLower)
import           Lens.Micro                 ((^.))

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.Wreq
import           System.Console.Haskeline

data Env = Env { txsHost   :: String
               , sessionId :: Int
               }

type EnvM = ReaderT Env IO

data TorXakisServerException = TorXakisServerException String Int String String
instance Show TorXakisServerException where
    show (TorXakisServerException body stat url expc) = concat
        [ "TorXakis server returned error: "
        , body
        , "\nHTTP Status: "
        , show stat
        , "\nURL: "
        , url
        , "\nExpected: "
        , expc
        ]
instance Exception TorXakisServerException

startCLI :: EnvM ()
startCLI = runInputT defaultSettings cli
  where
    cli :: InputT EnvM ()
    cli = do
        printer <- getExternalPrint
        sid <- lift $ asks sessionId
        liftIO $ printer $ "SessionId: " ++ show sid
        liftIO $ printer "Starting printer async..."
        pa <- liftIO $ async $ timer 0 printer
        handleInterrupt (return ())
                        $ withInterrupt loop
        liftIO $ cancel pa
    loop :: InputT EnvM ()
    loop = do
        minput <- getInputLine "% "
        case minput of
            Nothing -> return ()
            Just "" -> loop
            Just "quit" -> return ()
            Just input -> do outputStrLn $ "Input was: " ++ input
                             dispatch input
                             loop
    timer :: Int -> (String -> IO ()) -> IO ()
    timer n p = do
        threadDelay (10 ^ (6 :: Int))
        p $ "Printer waiting: " ++ show n
        timer (n+1) p
    dispatch :: String -> InputT EnvM ()
    dispatch inputLine = do
        let tokens = words inputLine
            cmd = head tokens
        host <- lift $ asks txsHost
        case map toLower cmd of
            "info" -> getTxsInfo
            "load" -> outputStrLn $ "Executing load cmd to host: " ++ host
            _      -> outputStrLn $ "Unknown command: " ++ cmd
          where
            getTxsInfo :: InputT EnvM () -- todo: split to modules?
            getTxsInfo = do
                host <- lift $ asks txsHost
                let url = host ++ "info"
                r <- (lift . lift) $ get url
                let st = r ^. responseStatus . statusCode
                case st of
                    200 ->
                        let
                            parseInfo :: Object -> Parser (String, String)
                            parseInfo o = do
                                v  <- o .: "version" :: Parser String
                                bt <- o .: "buildTime"   :: Parser String
                                return (v,bt)
                            info = do infoObj <- decode $ r ^. responseBody
                                      parseMaybe parseInfo infoObj
                        in case info of
                            Nothing     -> throwIO $ TorXakisServerException
                                                (BSL.unpack $ r ^. responseBody)
                                                st url "JSON - Version and BuildTime"
                            Just (v,bt) -> do
                                            outputStrLn $ concat
                                                [ "Torxakis version: ", v
                                                , "\nBuild time: ", bt]
                                            return ()

                    _   -> throwIO $ TorXakisServerException
                                    (BSL.unpack $ r ^. responseBody)
                                    st url "HTTP Status 200"
