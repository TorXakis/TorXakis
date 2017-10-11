{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.RealWorld (benchmarksSet) where

import           Benchmarks.Common
import           Examples.Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "RealWorld"

multipleControlLoops :: TxsExample
multipleControlLoops = TxsExample
    { exampleName = "Multiple Control Loops Stepper"
    , txsModelFiles = [ txsFilePathBench benchDir "MultipleControlLoops" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "MultipleControlLoops"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

customersAndOrders :: TxsExample
customersAndOrders = TxsExample
    { exampleName = "Customers and Orders"
    , txsModelFiles = [ txsFilePathBench benchDir "CustomersOrders" ]
    , txsCmdsFiles = [ txsCmdPathBench benchDir "CustomersOrders" ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

movingArms :: TxsExample
movingArms = TxsExample
    { exampleName = "Moving Arms"
    , txsModelFiles = [ txsFilePathBench benchDir "MovingArms" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "MovingArms"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

movingArmsPurpose :: TxsExample
movingArmsPurpose = TxsExample
    { exampleName = "Moving Arms (Purpose)"
    , txsModelFiles = [ txsFilePathBench benchDir "MovingArms" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPathBench benchDir "MovingArmsPurpose"
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
