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

readWriteConflictTestPurpText :: Text
readWriteConflictTestPurpText = "ReadWriteTestPurposes"

readWriteConflictAdvancedText :: Text
readWriteConflictAdvancedText = "ReadWriteAdvanced"

exampDir :: FilePath
exampDir = "ReadWriteConflict"

testStepper :: TxsExample
testStepper = TxsExample
  { exampleName = "Stepper Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

testPurpStepper :: TxsExample
testPurpStepper = TxsExample
  { exampleName = "Stepper Test for Purp Model"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictTestPurpText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }
  
testHitAll7 :: TxsExample
testHitAll7 = TxsExample
  { exampleName = "HitAll7 Purpose Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictTestPurpText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_HitAll7Purpose_Tester"]
  , txsServerArgs = []
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "ReadWrite_Simulator")
  , expectedResult = Pass
  }

testAdvancedStepper :: TxsExample
testAdvancedStepper = TxsExample
  { exampleName = "Advanced Stepper Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictAdvancedText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testStepper, testPurpStepper,testHitAll7,testAdvancedStepper]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "ReadWrite Conflict" examples
