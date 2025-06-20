{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Enable (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Enable"

modelFiles :: [FilePath]
modelFiles = [ insqatt $ txsFilePath BenchTest benchDir "Enable"
             , insqatt $ txsFilePath BenchTest "Sequence" "SingleActionSequence"
             ]

seqEnable :: TxsExample
seqEnable = emptyExample
    { exampleName = "sequence of enable operators, without data"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "SeqEnable"
                     ]
    , expectedResult = Pass
    }

seqEnableInt :: TxsExample
seqEnableInt = emptyExample
    { exampleName = "sequence of enable operators, with integers"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "SeqEnableInt"
                     ]
    , expectedResult = Pass
    }

seqEnableTwoInts :: TxsExample
seqEnableTwoInts = emptyExample
    { exampleName = "sequence of enable operators, with integers and two outputs"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "SeqEnableTwoInts"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Enable" [ seqEnable
                                       , seqEnableInt
                                       , seqEnableTwoInts
                                       ]
