{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Parallel (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Parallel"

modelFiles :: [FilePath]
modelFiles = [ insqatt $ txsFilePath BenchTest benchDir "Parallel"
             , insqatt $ txsFilePath BenchTest "Sequence" "SingleActionSequence"
             ]

parallel4 :: TxsExample
parallel4 = emptyExample
    { exampleName = "4 parallel sequential-processes"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "Parallel4"
                     ]
    , expectedResult = Pass
    }

parallelIStep4 :: TxsExample
parallelIStep4 = emptyExample
    { exampleName = "4 parallel sequential-processes, with internal step"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "Parallel4"
                     ]
    , expectedResult = Pass
    }


parallelAlternate4 :: TxsExample
parallelAlternate4 = emptyExample
    { exampleName = "4 parallel sequential-processes, with alternating actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ParallelAlternate4"
                     ]
    , expectedResult = Pass
    }

parallelMultiact4 :: TxsExample
parallelMultiact4 = emptyExample
    { exampleName = "4 parallel sequential-processes, with multiple actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ParallelMultiAct"
                     ]
    , expectedResult = Pass
    }

parallelSync :: TxsExample
parallelSync = emptyExample
    { exampleName = "convoluted parallel-synchronous model"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ParallelSync"
                     ]
    , expectedResult = Fail
    }

parallelNested :: TxsExample
parallelNested = emptyExample
    { exampleName = "4 parallel nested synchronizing sequences"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ParallelNested"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Parallel" [ parallel4
                                         , parallelIStep4
                                         , parallelAlternate4
                                         , parallelMultiact4
                                         , parallelSync
                                         , parallelNested
                                         ]
