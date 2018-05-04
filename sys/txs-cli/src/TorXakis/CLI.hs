{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.CLI
    ( startCLI
    , module TorXakis.CLI.Env
    , runCli
    )
where

import           Control.Arrow              ((|||))
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async, cancel)
import           Control.Lens               ((^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask, asks)
import           Control.Monad.Trans        (lift)
import           Data.Aeson                 (decode)
import           Data.Aeson.Types           (Object, Parser, parseMaybe, (.:))
import           Data.Char                  (toLower)

import qualified Data.ByteString.Lazy.Char8 as BSL

import           Network.Wreq
import           System.Console.Haskeline

import           TorXakis.CLI.Conf
import           TorXakis.CLI.Env
import           TorXakis.CLI.Log
import           TorXakis.CLI.WebClient

-- | Client monad
newtype CLIM a = CLIM { runCli :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadException)

startCLI :: CLIM ()
startCLI = runInputT defaultSettings cli
  where
    cli :: InputT CLIM ()
    cli = do
        -- Set the debugger
        initLogger
        printer <- getExternalPrint
        sid <- lift $ asks sessionId
        info $ "SessionId: " ++ show sid
        info "Starting printer async..."
        pa <- liftIO $ async $ timer 0 printer
        handleInterrupt (return ())
                        $ withInterrupt loop
        liftIO $ cancel pa
    loop :: InputT CLIM ()
    loop = do
        minput <- getInputLine (defaultConf ^. prompt)
        case minput of
            Nothing -> return ()
            Just "" -> loop
            Just "quit" -> return ()
            Just input -> do dispatch input
                             loop
    timer :: Int -> (String -> IO ()) -> IO ()
    timer n p = do
        -- TODO: Connect and wait for SSEs, print whatever comes
        --       use foldGet as in Stepper test in sys\txs-webserver\test\Spec.hs
        --       or in testTorXakisWithEcho example in sys\txs-lib\src\TorXakis\Lib\Examples.hs
        info "Hello, I'm the printer"
        threadDelay (2 * (10 ^ (6 :: Int)))
        timer (n+1) p
    dispatch :: String -> InputT CLIM ()
    dispatch inputLine = do
        let tokens = words inputLine
            cmd = head tokens
        case map toLower cmd of
            "info"    -> getTxsInfo
            "load"    -> loadFile $ tail tokens
            "stepper" -> initStepper $ tail tokens
            _         -> outputStrLn $ "Unknown command: " ++ cmd
          where
            getTxsInfo :: InputT CLIM () -- todo: split to modules?
            getTxsInfo = do
                host <- lift $ asks txsHost
                let url = host ++ "info"
                r <- liftIO $ get url
                let st = r ^. responseStatus . statusCode
                case st of
                    200 -> -- OK
                        let
                            parseInfo :: Object -> Parser (String, String)
                            parseInfo o = do
                                v  <- o .: "version" :: Parser String
                                bt <- o .: "buildTime"   :: Parser String
                                return (v,bt)
                            info' = do infoObj <- decode $ r ^. responseBody
                                       parseMaybe parseInfo infoObj
                        in case info' of
                            Nothing     -> undefined-- throwIO $ TorXakisServerException
                                                -- (BSL.unpack $ r ^. responseBody)
                                                -- st url "JSON - Version and BuildTime"
                            Just (v,bt) -> do
                                            outputStrLn $ concat
                                                [ "Torxakis version: ", v
                                                , "\nBuild time: ", bt]
                                            return ()
                    _   -> outputStrLn (BSL.unpack $ r ^. responseBody)
            loadFile :: [FilePath] -> InputT CLIM ()
            loadFile files = do
                env <- lift ask
                res <- load env files
                (outputStrLn ||| skip) res
            initStepper :: [String] -> InputT CLIM ()
            initStepper [mName] = do
                env <- lift ask
                res <- stepper env mName
                (outputStrLn ||| skip) res
            initStepper _ = outputStrLn "This command is not supported yet."

skip :: a -> InputT CLIM ()
skip _ = return ()
