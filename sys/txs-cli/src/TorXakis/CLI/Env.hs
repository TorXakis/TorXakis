{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | Environment of the TorXakis CLI.
module TorXakis.CLI.Env where

import           Control.Exception.Safe (Exception)

data Env = Env { txsHost   :: String
               , sessionId :: Int
               }

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
