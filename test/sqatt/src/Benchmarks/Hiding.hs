{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Hiding (benchmarksSet) where

import           Benchmarks.Common
import           Examples.Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Hiding"

modelFiles :: [FilePath]
modelFiles = [ txsFilePathBench benchDir "Hiding"
             , txsFilePathBench "Sequence" "SingleActionSequence"
             ]

alt4hide1 :: TxsExample
alt4hide1 = TxsExample
    { exampleName = "Alternate 4 hide 1 action"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Alternate4Hide1Act"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

hideFirstSFA :: TxsExample
hideFirstSFA = TxsExample
    { exampleName = "Hide first of sync first alternate"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "HideFirstSFA"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

hideSecondSFA :: TxsExample
hideSecondSFA = TxsExample
    { exampleName = "Hide second of sync second alternate"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "HideSecondSFA"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

matchNoData :: TxsExample
matchNoData = TxsExample
    { exampleName = "Match"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "Match"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

matchInt :: TxsExample
matchInt = TxsExample
    { exampleName = "Match Int"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "MatchInt"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Hiding" [ alt4hide1
                                       , hideFirstSFA
                                       , hideSecondSFA
                                       , matchNoData
                                       , matchInt ]
