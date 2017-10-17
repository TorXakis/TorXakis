{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Integration.Any (testSet) where

import           Paths
import           Prelude hiding (FilePath)
import           Sqatt

testDir :: FilePath
testDir = "Any"

anyTest :: TxsExample
anyTest = TxsExample
    { exampleName = "Test 0"
    , txsModelFiles = [ txsFilePath ITest testDir "anyTest"]
    , txsCmdsFiles = [ txsCmdPath ITest testDir "anyTest"]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

testSet :: TxsExampleSet
testSet = TxsExampleSet "Any" [ anyTest ]
