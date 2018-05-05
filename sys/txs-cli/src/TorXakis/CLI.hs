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
import           Control.Concurrent       (newChan, readChan)
import           Control.Concurrent.Async (async, cancel)
import           Control.Lens             ((^.))
import           Control.Monad            (forever)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, asks,
                                           runReaderT)
import           Control.Monad.Trans      (lift)
import qualified Data.ByteString.Char8    as BS
import           Data.Char                (toLower)
import qualified Data.Text                as T
import           System.Console.Haskeline
import           Text.Read                (readMaybe)

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
startCLI = do
    runInputT defaultSettings cli
  where
    cli :: InputT CLIM ()
    cli = do
        Log.initL
        sId <- lift $ asks sessionId
        Log.info $ "SessionId: " ++ show sId
        Log.info "Starting printer async..."
        printer <- getExternalPrint
        ch <- liftIO newChan
        env <- lift ask
        producer <- liftIO $ async $
            sseSubscribe env ch $ concat ["sessions/", show sId, "/messages"]
        consumer <- liftIO $ async $ forever $ do
            msg <- readChan ch
            printer $ "<< " ++ BS.unpack msg
        handleInterrupt (return ())
                        $ withInterrupt loop
        liftIO $ cancel producer
        liftIO $ cancel consumer

    loop :: InputT CLIM ()
    loop = do
        minput <- getInputLine (defaultConf ^. prompt)
        case minput of
            Nothing -> return ()
            Just "" -> loop
            Just "quit" -> return ()
            Just input -> do dispatch input
                             loop
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
                subStepper (tail tokens) >>= output
            "step" ->
                subStep (tail tokens) >>= output
            _         ->
                output $ "Unknown command: " ++ cmd
          where
            -- | Sub-command stepper.
            subStepper :: [String] -> InputT CLIM ()
            subStepper [mName] =
                lift (stepper mName) >>= output
            subStepper _ = outputStrLn "This command is not supported yet."
            -- | Sub-command step.
            subStep [with] = do
                case readMaybe with of
                    Nothing -> outputStrLn "Number of steps should be an integer."
                    Just n  -> lift (step n) >>= output
            subStep _ = outputStrLn "This command is not supported yet."

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
