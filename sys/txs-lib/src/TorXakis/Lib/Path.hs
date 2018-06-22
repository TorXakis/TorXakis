{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Lib.Path where

import           Control.Monad.State     (lift)
import           Data.Semigroup          ((<>))
import qualified Data.Text               as T

import qualified TxsCore                 as Core
import           TxsShow                 (fshow, showN)

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session
import           TorXakis.Lib.ToProcdef

-- | Return the path followed until current state.
pathDump :: Session -> IO (Response String)
pathDump s = do
    pathRes <- runResponse $ lift $ runIOC s Core.txsPath
    return $ case pathRes of
        Right path ->
            Right $ unlines [ concat [ showN stp 6
                                    , ": "
                                    , show s1
                                    , " -> "
                                    , unwords $ lines $ fshow act
                                    , " -> "
                                    , show s2
                                    ]
                            | (stp, (s1, act, s2)) <- zip [1..] path
                            ]
        Left err -> Left $ "Error: " <> err

-- | Return trace of the path followed until current state.
traceDump :: Session -> String -> IO (Response String)
traceDump s traceAs = do
    pathRes <- runResponse $ lift $ runIOC s Core.txsPath
    return $ case pathRes of
        Right path ->
            let trace = [ a | (_, a ,_) <- path ]
            in  case traceAs of
                ""      -> Right $ unlines [ concat [ showN stp 6
                                                    , ": "
                                                    , unwords $ lines $ fshow act
                                                    ]
                                            | (stp, act) <- zip [1..] trace
                                            ]
                "proc"  -> Right $ unlines [T.unpack (toProcdef trace)]
                "purp"  -> Right $ unlines [T.unpack (toPurpdef trace)]
                _       -> Right $ "No such trace format: " ++ traceAs
        Left err -> Left $ "Error: " <> err
