{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Hiding (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Hiding"

modelFiles :: [FilePath]
modelFiles = [ insqatt $ txsFilePath BenchTest benchDir "Hiding"
             , insqatt $ txsFilePath BenchTest "Sequence" "SingleActionSequence"
             ]

alt4hide1 :: TxsExample
alt4hide1 = emptyExample
    { exampleName = "alternate 4 hide 1 action"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "Alternate4Hide1Act"
                     ]
    , expectedResult = Pass
    }

hideFirstSFA :: TxsExample
hideFirstSFA = emptyExample
    { exampleName = "hide first of sync first alternate"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "HideFirstSFA"
                     ]
    , expectedResult = Pass
    }

hideSecondSFA :: TxsExample
hideSecondSFA = emptyExample
    { exampleName = "hide second of sync second alternate"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "HideSecondSFA"
                     ]
    , expectedResult = Pass
    }

matchNoData :: TxsExample
matchNoData = emptyExample
    { exampleName = "match"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "Match"
                     ]
    , expectedResult = Pass
    }

matchInt :: TxsExample
matchInt = emptyExample
    { exampleName = "match Int"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "MatchInt"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Hiding" [ alt4hide1
                                       , hideFirstSFA
                                       , hideSecondSFA
                                       , matchNoData
                                       , matchInt ]
