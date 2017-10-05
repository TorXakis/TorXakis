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

modelFiles :: [FilePath]
modelFiles = [txsFilePathBench benchDir "SingleActionSequence"]

do100Acts :: TxsExample
do100Acts = TxsExample
    { exampleName = "100 actions"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "SingleActionSequence"
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100IActs :: TxsExample
do100IActs = TxsExample
    { exampleName = "100 internal actions"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "SingleActionIStepSequence"
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100DataActs :: TxsExample
do100DataActs = TxsExample
    { exampleName = "100 data actions"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ForeverOutput4"
    , sutExample = Nothing
    , expectedResult = Pass
    }

sequence10Ints :: TxsExample
sequence10Ints = TxsExample
    { exampleName = "sequence with a 10 integer channel"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "Sequence10Ints"
    , sutExample = Nothing
    , expectedResult = Pass
    }

sequence10IntsTD :: TxsExample
sequence10IntsTD = TxsExample
    { exampleName = "sequence with a 10 integer channel, using a custom type"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "Sequence10IntsTypeDef"
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Sequence" [ do100Acts
                                         , do100IActs
                                         , do100DataActs
                                         , sequence10Ints
                                         , sequence10IntsTD
                                         ]
