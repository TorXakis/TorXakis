{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Hiding (benchmarksSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Hiding"

matchInt :: TxsExample
matchInt = TxsExample
    { exampleName = "Match Int"
    , txsModelFiles = [txsFilePathBench benchDir "Hiding"]
    , txsCommandsFile = txsCmdPathBench benchDir "MatchInt"
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Hiding" [ matchInt ]


