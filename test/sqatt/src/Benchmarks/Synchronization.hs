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

-- do100Acts3procs :: TxsExample
-- do100Acts3procs = TxsExample
--     { exampleName = "3 processes"
--     , txsModelFiles = modelFiles
--     , txsCommandsFile = txsCmdPathBench benchDir "ForeverSynchronized3"
--     , sutExample = Nothing
--     , expectedResult = Pass
--     }

-- do100Acts6procs :: TxsExample
-- do100Acts6procs = TxsExample
--     { exampleName = "6 processes"
--     , txsModelFiles = modelFiles
--     , txsCommandsFile = txsCmdPathBench benchDir "ForeverSynchronized6"
--     , sutExample = Nothing
--     , expectedResult = Pass
--     }

alternateTwoProcs :: TxsExample
alternateTwoProcs = TxsExample
    { exampleName = "alternate 2 processes"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ForeverSyncAlt2"
    , sutExample = Nothing
    , expectedResult = Pass
    }


-- do100Acts3procsIStep :: TxsExample
-- do100Acts3procsIStep = TxsExample
--     { exampleName = "3 processes with internal action"
--     , txsModelFiles = modelFiles
--     , txsCommandsFile = txsCmdPathBench benchDir "ForeverSynchronizedIStep3"
--     , sutExample = Nothing
--     , expectedResult = Pass
--     }

manyActsSyncTop :: TxsExample
manyActsSyncTop = TxsExample
    { exampleName = "many processes synchronizing at the top"
    , txsModelFiles = modelFiles
    , txsCommandsFile = txsCmdPathBench benchDir "ManyActsSyncTop"
    , sutExample = Nothing
    , expectedResult = Pass
    }



benchmarksSet :: TxsExampleSet
-- TODO: do100Acts3procs hangs TorXakis, see https://github.com/TorXakis/TorXakis/issues/268
-- benchmarksSet = TxsExampleSet "Synchronization" [ do100Acts3procs
--                                                 , do100Acts6procs
--                                                 , alternateTwoProcs
--                                                 ]
benchmarksSet = TxsExampleSet "Synchronization" [ alternateTwoProcs
                                                , manyActsSyncTop
                                                ]


