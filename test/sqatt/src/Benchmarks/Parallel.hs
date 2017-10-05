{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Parallel (benchmarksSet) where

import           Benchmarks.Common
import           Examples.Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Parallel"

modelFiles :: [FilePath]
modelFiles = [ txsFilePathBench benchDir "Parallel"
             , txsFilePathBench "Sequence" "SingleActionSequence"
             ]

parallel4 :: TxsExample
parallel4 = TxsExample
    { exampleName = "4 parallel sequential-processes"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Parallel4"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

parallelIStep4 :: TxsExample
parallelIStep4 = TxsExample
    { exampleName = "4 parallel sequential-processes, with internal step"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Parallel4"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }


parallelAlternate4 :: TxsExample
parallelAlternate4 = TxsExample
    { exampleName = "4 parallel sequential-processes with alternating actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "ParallelAlternate4"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

parallelMultiact4 :: TxsExample
parallelMultiact4 = TxsExample
    { exampleName = "4 parallel sequential-processes with multiple actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "ParallelMultiAct"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

parallelSync :: TxsExample
parallelSync = TxsExample
    { exampleName = "convoluted parallel-synchronous model"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "ParallelSync"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Fail
    }

parallelNested :: TxsExample
parallelNested = TxsExample
    { exampleName = "4 parallel nested synchronizing sequences"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "ParallelNested"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
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
