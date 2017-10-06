{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Sequence (benchmarksSet) where

import           Benchmarks.Common
import           Examples.Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Sequence"

modelFiles :: [FilePath]
modelFiles = [txsFilePathBench benchDir "SingleActionSequence"]

do100Acts :: TxsExample
do100Acts = TxsExample
    { exampleName = "100 actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "SingleActionSequence"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100IActs :: TxsExample
do100IActs = TxsExample
    { exampleName = "100 internal actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "SingleActionIStepSequence"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

do100DataActs :: TxsExample
do100DataActs = TxsExample
    { exampleName = "100 data actions"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "ForeverOutput4"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

sequence10Ints :: TxsExample
sequence10Ints = TxsExample
    { exampleName = "sequence with a 10 integer channel"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Sequence10Ints"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

sequence10IntsCvc4 :: TxsExample
sequence10IntsCvc4 = TxsExample
    { exampleName = "sequence with a 10 integer channel (cvc4)"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Sequence10Ints"
                     ]
    , txsServerArgs = ["--smt-solver", "cvc4"]
    , sutExample = Nothing
    , expectedResult = Pass
    }

sequence10IntsTD :: TxsExample
sequence10IntsTD = TxsExample
    { exampleName = "sequence with a 10 integer channel, using a custom type"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Sequence10IntsTypeDef"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Sequence" [ do100Acts
                                         , do100IActs
                                         , do100DataActs
                                         , sequence10Ints
                                         , sequence10IntsCvc4
                                         , sequence10IntsTD
                                         ]
