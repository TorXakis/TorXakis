{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Data.String.Utils     (strip)
import           System.Console.Docopt (Docopt, docopt, getArg, longOption,
                                        parseArgsOrExit)
import           System.Environment    (getArgs)
import           Text.Read             (readMaybe)

import           API

patterns :: Docopt
patterns = [docopt|
txs-webserver

Usage: txs-webserver [options]

Options:
-p --port <number>    Port number in which the server will be started. If
                      ommited port 8080 will be used.
|]


main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    -- Determine the port number.
    let p = maybe "8080" strip (args `getArg` longOption "port")
        portErr =
            print $ "The port number should be an integer between 0 and 65535, got " ++ show p
    case readMaybe p of
        Nothing -> portErr
        Just n  -> do
            putStrLn $ "Starting txs-webserver at port: " ++ show p
            if 0 <= n && n <= 65535
                then startApp n
                else portErr
