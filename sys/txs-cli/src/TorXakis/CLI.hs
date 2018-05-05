{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module TorXakis.CLI
    ( startCLI
    , module TorXakis.CLI.Env
    , runCli
    )
where

import           Control.Arrow            ((|||))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.Lens             ((^.))
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT, asks,
                                           runReaderT)
import           Control.Monad.Trans      (lift)
import           Data.Char                (toLower)
import qualified Data.Text                as T
import           System.Console.Haskeline

import           TorXakis.CLI.Conf
import           TorXakis.CLI.Env
import qualified TorXakis.CLI.Log         as Log
import           TorXakis.CLI.WebClient

-- | Client monad
newtype CLIM a = CLIM { innerM :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadException)

runCli :: Env -> CLIM a -> IO a
runCli e clim =
    runReaderT (innerM clim) e

startCLI :: CLIM ()
startCLI = runInputT defaultSettings cli
  where
    cli :: InputT CLIM ()
    cli = do
        Log.initL
        printer <- getExternalPrint
        sid <- lift $ asks sessionId
        Log.info $ "SessionId: " ++ show sid
        Log.info "Starting printer async..."
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
        Log.info "Hello, I'm the printer"
        threadDelay (2 * (10 ^ (6 :: Int)))
        timer (n+1) p
    dispatch :: String -> InputT CLIM ()
    dispatch inputLine = do
        let tokens = words inputLine
            cmd = head tokens
        case map toLower cmd of
            "info"    ->
                lift (runExceptT info) >>= output
            "load"    ->
                lift (load (tail tokens)) >>= output
            "stepper" ->
                initStepper (tail tokens) >>= output
            _         ->
                output $ "Unknown command: " ++ cmd
          where
            initStepper :: [String] -> InputT CLIM ()
            initStepper [mName] =
                lift (stepper mName) >>= output
            initStepper _ = outputStrLn "This command is not supported yet."

-- | Values that can be output in the command line.
class Outputable v where
    output :: v -> InputT CLIM ()

-- data Output v = forall v . Outputable v => Output v

-- outputRes :: forall v . Outputable v => v -> InputT CLIM ()
-- outputRes = output

instance Outputable () where
    output _ = return ()

instance Outputable String where
    output = outputStrLn

instance Outputable Info where
    output i = do
        outputStrLn $ "Version: " ++ T.unpack (i ^. version)
        outputStrLn $ "Build time: "++ T.unpack (i ^. buildTime)

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    output = output ||| output
