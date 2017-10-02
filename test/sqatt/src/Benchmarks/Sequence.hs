{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Sequence (benchmarksSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Sequence"

do100Acts :: TxsExample
do100Acts = TxsExample
    { exampleName = "100 actions"
    , txsModelFiles = [txsFilePathBench benchDir "SingleActionSequence"]
    , txsCommandsFile = txsCmdPathBench benchDir "SingleActionSequence"
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100IActs :: TxsExample
do100IActs = TxsExample
    { exampleName = "100 internal actions"
    , txsModelFiles = [txsFilePathBench benchDir "SingleActionSequence"]
    , txsCommandsFile = txsCmdPathBench benchDir "SingleActionIStepSequence"
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100DataActs :: TxsExample
do100DataActs = TxsExample
    { exampleName = "100 data actions"
    , txsModelFiles = [txsFilePathBench benchDir "SingleActionSequence"]
    , txsCommandsFile = txsCmdPathBench benchDir "ForeverOutput4"
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Sequence" [ do100Acts
                                         , do100IActs
                                         , do100DataActs
                                         ]
