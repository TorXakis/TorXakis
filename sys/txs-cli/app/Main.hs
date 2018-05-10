{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Control.Arrow              ((|||))
import           Control.Concurrent.Async   (race)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List                  (isInfixOf)
import           Data.String.Utils          (strip)
import           Lens.Micro                 ((^.), (^?))
import           Lens.Micro.Aeson           (key, _Integer)
import           Network.Wreq
import           System.Console.Docopt      (Docopt, docopt, getArg, longOption,
                                             parseArgsOrExit)
import           System.Environment         (getArgs)
import           System.Process             (readProcessWithExitCode)

import           TorXakis.CLI

patterns :: Docopt
patterns = [docopt|
ticl

Usage:
ticl [options]

Options:
-s --server <address>    Address of the server. If not present a new server
                         will be started at localhost, with a dynamically
                         assigned port.
|]

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    case strip <$> args `getArg` longOption "server" of
        -- TODO: make the user input type-safe. For instance we're assuming the host ends with a '/'.
        Nothing    ->
            tryStartCli (8000 :: Int)
        Just oAddr ->
            startCLIWithHost oAddr
    where
      -- | Try to start a web-server on the given port. Retry if the port was busy.
      tryStartCli n
          | n <= maxPortNumber = do
                let p = show n
                r <- race
                    (readProcessWithExitCode "txs-webserver" ["-p", p] "")
                    (startCLIWithHost ("http://localhost:" ++ p ++ "/"))
                -- If the server exits we get an error, so we retry.
                retryIfPortBusy ||| return $ r
          | otherwise =
              putStrLn "Unable to find a free port for launching the TorXakis webserver"
          where
            maxPortNumber = 2 ^ (16 :: Int) - 1
            retryIfPortBusy (_, _, err)
                | "resource busy" `isInfixOf` err =
                      -- The port is busy, we retry on the next one.
                      tryStartCli (n + 1)
                | otherwise =
                      -- We got some other kind of error. Giving up.
                      putStrLn
                      $  "Unexpected error when trying to "
                      ++ "start the TorXakis webserver: " ++ show err
      startCLIWithHost host = do
          initRes <- initTorXakisSession host -- todo: get session id from arguments
          case initRes of
              Left  e   -> print e
              Right sid -> runCli (Env host sid) startCLI
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


