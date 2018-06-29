{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Control.Concurrent.STM.TVar (newTVarIO)
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.List                   (isInfixOf)
import           Data.String.Utils           (strip)
import           GHC.IO.Handle               (hGetContents)
import           Lens.Micro                  ((^.), (^?))
import           Lens.Micro.Aeson            (key, _Integer)
import           Network.Wreq
import           System.Console.Docopt       (Docopt, argument, docopt,
                                              getAllArgs, getArg, longOption,
                                              parseArgsOrExit)
import           System.Environment          (getArgs)
import           System.Process              (StdStream (NoStream), proc,
                                              std_out, withCreateProcess)
-- import           System.Process             (proc, withCreateProcess)

import           TorXakis.CLI
import qualified TorXakis.CLI.Log            as Log

patterns :: Docopt
patterns = [docopt|
ticl

Usage: ticl [options] [<model-files>...]

Options:
-s --server <address>    Address of the server. If not present a new server
                         will be started at localhost, with a dynamically
                         assigned port.
|]

-- | The CLI will start the web-server on a dynamically allocated port if no
-- host is specified. The port numbers are tried one by one starting from (the
-- rather arbitrary number 8000), till one free port is found, or until the
-- maximum port number (2^16 - 1) is reached.
main :: IO ()
main = do
    Log.initL
    as <- getArgs
    Log.info $ "Starting with arguments: " ++ show as
    args <- parseArgsOrExit patterns as
    let modelFiles = args `getAllArgs` argument "model-files"
    Log.info $ "Model files from args: " ++ show modelFiles
    case strip <$> args `getArg` longOption "server" of
        -- TODO: make the user input type-safe. For instance we're assuming the host ends with a '/'.
        Nothing    ->
            tryStartCli (8080 :: Int) modelFiles
        Just oAddr ->
            startCLIWithHost oAddr modelFiles
    where
      -- | Try to start a web-server on the given port. Retry if the port was busy.
      tryStartCli n mfs
          | n <= maxPortNumber = do
                let p = show n
                Log.info $ "Running web server on port " ++ p
                withCreateProcess (proc "txs-webserver" ["-p", p]) {std_out = NoStream} $
                    \_stdin _stdout stderr _ph -> do
                        -- If the server exits we get an error, so we retry.
                        errStr  <-  case stderr of
                                        Nothing -> do
                                            Log.info "No stderr handle"
                                            return []
                                        Just h  -> do
                                            Log.info "Reading stderr handle"
                                            hGetContents h
                        Log.info $ "Read stderr handle: " ++ errStr
                        if null errStr
                            then startCLIWithHost ("http://localhost:" ++ p ++ "/") mfs
                            else retryIfPortBusy errStr
          | otherwise = let errStr = "Unable to find a free port for launching the TorXakis webserver"
                        in Log.warn errStr
          where
            maxPortNumber = 2 ^ (16 :: Int) - 1
            retryIfPortBusy err
                | "resource busy" `isInfixOf` err =
                      -- The port is busy, we retry on the next one.
                      tryStartCli (n + 1) mfs
                | otherwise =
                      -- We got some other kind of error. Giving up.
                      Log.warn
                      $  "Unexpected error when trying to "
                      ++ "start the TorXakis webserver: " ++ show err
      startCLIWithHost host mfs = do
          Log.info "Initializing TorXakis session..."
          Log.info "Starting CLI"
          initRes <- initTorXakisSession host -- todo: get session id from arguments
          case initRes of
              Left  e   -> Log.warn $ show e
              Right sid -> do
                            Log.info $ "Starting CLI for session" ++ show sid
                            noHdl <- newTVarIO Nothing
                            notWaiting <- newTVarIO False
                            runCli (Env host sid noHdl notWaiting) (startCLI mfs)
                            Log.info "Exiting CLI"
      initTorXakisSession :: String -> IO (Either TorXakisServerException Int)
      initTorXakisSession host = do
          let url = host ++ "sessions/new"
          r <- post url [partText "" ""]
          let st = r ^. responseStatus . statusCode
          return $
              case st of
                  201 ->
                      case r ^? responseBody . key "sessionId" . _Integer of
                          Just sid -> Right $ fromIntegral sid
                          _        -> Left $ TorXakisServerException
                                      (BSL.unpack $ r ^. responseBody)
                                      st url "JSON - sessionId"
                  _   -> Left $ TorXakisServerException
                                (BSL.unpack $ r ^. responseBody)
                                st url "HTTP Status 201"

