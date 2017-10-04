{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Synchronization (benchmarksSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Synchronization"

modelFiles :: [FilePath]
modelFiles = [ txsFilePathBench benchDir "Synchronization"
             , txsFilePathBench "Sequence" "SingleActionSequence"
             ]

do100Acts3procs :: TxsExample
do100Acts3procs = TxsExample
    { exampleName = "3 processes"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ForeverSynchronized3"
    , sutExample = Nothing
    , expectedResult = Pass
    }

alternateTwoProcs :: TxsExample
alternateTwoProcs = TxsExample
    { exampleName = "alternate 2 processes"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ForeverSyncAlt2"
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100Acts3procsIStep :: TxsExample
do100Acts3procsIStep = TxsExample
    { exampleName = "3 processes with internal action"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ForeverSynchronizedIStep3"
    , sutExample = Nothing
    , expectedResult = Pass
    }

manyActsSyncTop :: TxsExample
manyActsSyncTop = TxsExample
    { exampleName = "many processes synchronizing at the top"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ManyActsSyncTop"
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Synchronization" [ alternateTwoProcs
                                                , do100Acts3procs
                                                , do100Acts3procsIStep
                                                , manyActsSyncTop
                                                ]