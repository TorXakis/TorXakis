{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lens.Micro                 ((^.), (^?))
import           Lens.Micro.Aeson           (key, _Integer)
import           System.Process             (StdStream (NoStream), proc,
                                             std_out, withCreateProcess)

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.Wreq

import           TorXakis.CLI

main :: IO ()
main =
    let host = "http://localhost:8080/" -- todo: get server address from arguments
    in withCreateProcess (proc "txs-webserver-exe" []) {std_out = NoStream} $
        \_stdin _stdout _stderr _ph -> do
            initRes <- initTorXakisSession host -- todo: get session id from arguments
            case initRes of
                Left  e   -> print e
                Right sid -> runCli (Env host sid) startCLI
          where
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

