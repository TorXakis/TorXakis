{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Choice (benchmarksSet) where

import           Benchmarks.Common
import           Examples.Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Choice"

do100Choices :: TxsExample
do100Choices = TxsExample
    { exampleName = "100 choices"
    , txsModelFiles = [txsFilePathBench benchDir "Choice"]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "ForeverChoice"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Choice" [ do100Choices ]
