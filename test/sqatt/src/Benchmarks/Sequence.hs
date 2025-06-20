{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Sequence (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Sequence"

modelFiles :: [FilePath]
modelFiles = [insqatt $ txsFilePath BenchTest benchDir "SingleActionSequence"]

do100Acts :: TxsExample
do100Acts = emptyExample
    { exampleName = "100 actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "SingleActionSequence"
                     ]
    , expectedResult = Pass
    }

do100IActs :: TxsExample
do100IActs = emptyExample
    { exampleName = "100 internal actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "SingleActionIStepSequence"
                     ]
    , expectedResult = Pass
    }

do100DataActs :: TxsExample
do100DataActs = emptyExample
    { exampleName = "100 data actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "ForeverOutput4"
                     ]
    , expectedResult = Pass
    }

sequence10Ints :: TxsExample
sequence10Ints = emptyExample
    { exampleName = "sequence with a 10 integer channel"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "Sequence10Ints"
                     ]
    , expectedResult = Pass
    }

-- cvc4 not currently supported.
-- sequence10IntsCvc4 :: TxsExample
-- sequence10IntsCvc4 = emptyExample
--     { exampleName = "sequence with a 10 integer channel (cvc4)"
--     , txsModelFiles = modelFiles
--     , txsCmdsFiles = [ seedSetupCmdFile
--                      , insqatt $ txsCmdPath BenchTest benchDir "Sequence10Ints"
--                      ]
--     , txsServerArgs = ["--smt-solver", "cvc4"]
--       , expectedResult = Pass
--     }

sequence10IntsTD :: TxsExample
sequence10IntsTD = emptyExample
    { exampleName = "sequence with a 10 integer channel, using a custom type"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "Sequence10IntsTypeDef"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Sequence" [ do100Acts
                                         , do100IActs
                                         , do100DataActs
                                         , sequence10Ints
                                         --, sequence10IntsCvc4
                                         , sequence10IntsTD
                                         ]
