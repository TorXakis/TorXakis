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
multipleControlLoops = emptyExample
    { exampleName = "Multiple Control Loops Stepper"
    , txsModelFiles = [ insqatt $ txsFilePath BenchTest benchDir "MultipleControlLoops" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "MultipleControlLoops"
                     ]
    , expectedResult = Pass
    }

customersAndOrders :: TxsExample
customersAndOrders = emptyExample
    { exampleName = "Customers and Orders"
    , txsModelFiles = [ insqatt $ txsFilePath BenchTest benchDir "CustomersOrders" ]
    , txsCmdsFiles = [ insqatt $ txsCmdPath BenchTest benchDir "CustomersOrders" ]
    , expectedResult = Pass
    }

movingArms :: TxsExample
movingArms = emptyExample
    { exampleName = "Moving Arms"
    , txsModelFiles = [ insqatt $ txsFilePath BenchTest benchDir "MovingArms" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "MovingArms"
                     ]
    , expectedResult = Pass
    }

movingArmsPurpose :: TxsExample
movingArmsPurpose = emptyExample
    { exampleName = "Moving Arms (Purpose)"
    , txsModelFiles = [ insqatt $ txsFilePath BenchTest benchDir "MovingArms" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "MovingArmsPurpose"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "RealWorld" [ multipleControlLoops
                                          , customersAndOrders
                                          , movingArms
                                          , movingArmsPurpose
                                          ]
