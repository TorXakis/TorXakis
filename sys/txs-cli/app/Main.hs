{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Lens.Micro                 ((^.), (^?))
import           Lens.Micro.Aeson           (key, _Integer)
import           System.Process             (StdStream (NoStream),
                                             delegate_ctlc, proc, std_out,
                                             withCreateProcess)

import           Data.String.Utils          (strip)
import           System.Console.Docopt      (Docopt, docopt, getArg, longOption,
                                             parseArgsOrExit)
import           System.Environment         (getArgs)

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.Wreq

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
            withCreateProcess (proc "txs-webserver" ["-p", "8080"])
            { std_out = NoStream
            , delegate_ctlc = True
            } $ \_ _ _ _ ->
            startCLIWithHost "http://localhost:8080/"
        Just oAddr ->
            startCLIWithHost oAddr
    where
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

