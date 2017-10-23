{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.RealWorld (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "RealWorld"

multipleControlLoops :: TxsExample
multipleControlLoops = TxsExample
    { exampleName = "Multiple Control Loops Stepper"
    , txsModelFiles = [ txsFilePath BenchTest benchDir "MultipleControlLoops" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "MultipleControlLoops"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

customersAndOrders :: TxsExample
customersAndOrders = TxsExample
    { exampleName = "Customers and Orders"
    , txsModelFiles = [ txsFilePath BenchTest benchDir "CustomersOrders" ]
    , txsCmdsFiles = [ txsCmdPath BenchTest benchDir "CustomersOrders" ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

movingArms :: TxsExample
movingArms = TxsExample
    { exampleName = "Moving Arms"
    , txsModelFiles = [ txsFilePath BenchTest benchDir "MovingArms" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "MovingArms"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

movingArmsPurpose :: TxsExample
movingArmsPurpose = TxsExample
    { exampleName = "Moving Arms (Purpose)"
    , txsModelFiles = [ txsFilePath BenchTest benchDir "MovingArms" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "MovingArmsPurpose"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "RealWorld" [ multipleControlLoops
                                          , customersAndOrders
                                          , movingArms
                                          , movingArmsPurpose
                                          ]
