{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.ReadWriteConflict (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

readWriteConflictText :: Text
readWriteConflictText = "ReadWrite"

readWriteConflictAdvancedText :: Text
readWriteConflictAdvancedText = "ReadWriteAdvanced"

exampDir :: FilePath
exampDir = "ReadWriteConflict"

testStepper :: TxsExample
testStepper = TxsExample
  { exampleName = "ReadWriteConflict Stepper Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

testStepper7 :: TxsExample
testStepper7 = TxsExample
  { exampleName = "ReadWriteConflict Stepper7 Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite7_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

testAdvancedStepper :: TxsExample
testAdvancedStepper = TxsExample
  { exampleName = "ReadWriteConflict_Advanced Stepper Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictAdvancedText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testStepper,testStepper7,testAdvancedStepper]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Read/Write Conflict" examples
