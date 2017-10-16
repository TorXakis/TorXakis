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
modelFiles = [ txsFilePath BenchTest benchDir "Synchronization"
             , txsFilePath BenchTest "Sequence" "SingleActionSequence"
             ]

do100Acts3procs :: TxsExample
do100Acts3procs = TxsExample
    { exampleName = "3 processes"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "ForeverSynchronized3"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

alternateTwoProcs :: TxsExample
alternateTwoProcs = TxsExample
    { exampleName = "alternate 2 processes"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "ForeverSyncAlt2"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100Acts3procsIStep :: TxsExample
do100Acts3procsIStep = TxsExample
    { exampleName = "3 processes with internal action"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "ForeverSynchronizedIStep3"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

manySeqSync :: TxsExample
manySeqSync = TxsExample
    { exampleName = "6 sequential processes synchronizing in two actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "SyncAlternate6"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

manyActsSyncTop :: TxsExample
manyActsSyncTop = TxsExample
    { exampleName = "many processes synchronizing at the top"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "ManyActsSyncTop"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

manySyncPairs :: TxsExample
manySyncPairs = TxsExample
    { exampleName = "many processes synchronizing in pairs"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "ManySyncPairs"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
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
