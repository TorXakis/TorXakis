{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Synchronization (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Synchronization"

modelFiles :: [FilePath]
modelFiles = [ insqatt $ txsFilePath BenchTest benchDir "Synchronization"
             , insqatt $ txsFilePath BenchTest "Sequence" "SingleActionSequence"
             ]

do100Acts3procs :: TxsExample
do100Acts3procs = emptyExample
    { exampleName = "3 processes"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ForeverSynchronized3"
                     ]
    , expectedResult = Pass
    }

alternateTwoProcs :: TxsExample
alternateTwoProcs = emptyExample
    { exampleName = "alternate 2 processes"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ForeverSyncAlt2"
                     ]
    , expectedResult = Pass
    }

do100Acts3procsIStep :: TxsExample
do100Acts3procsIStep = emptyExample
    { exampleName = "3 processes with internal action"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ForeverSynchronizedIStep3"
                     ]
    , expectedResult = Pass
    }

manySeqSync :: TxsExample
manySeqSync = emptyExample
    { exampleName = "6 sequential processes synchronizing in two actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "SyncAlternate6"
                     ]
    , expectedResult = Pass
    }

manyActsSyncTop :: TxsExample
manyActsSyncTop = emptyExample
    { exampleName = "many processes synchronizing at the top"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ManyActsSyncTop"
                     ]
    , expectedResult = Pass
    }

manySyncPairs :: TxsExample
manySyncPairs = emptyExample
    { exampleName = "many processes synchronizing in pairs"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ManySyncPairs"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Synchronization" [ alternateTwoProcs
                                                , do100Acts3procs
                                                , do100Acts3procsIStep
                                                , manySeqSync
                                                , manyActsSyncTop
                                                , manySyncPairs
                                                ]
